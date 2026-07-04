/**
 * Commit Hooks Extension
 *
 * Intercepts `git commit` commands and runs pre-commit / post-commit
 * hook scripts automatically. Keeps lint/test output out of the main
 * agent session.
 *
 * Hook locations (run in order):
 *   ~/.pi/agent/skills/commit/hooks/pre-commit   (always)
 *   $PROJECT_ROOT/.pi/hooks/pre-commit            (if exists)
 *   ~/.pi/agent/skills/commit/hooks/post-commit  (always)
 *   $PROJECT_ROOT/.pi/hooks/post-commit           (if exists)
 *
 * Env vars passed to every hook:
 *   COMMIT_MESSAGE, VCS=git, PROJECT_ROOT
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { access } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";

const execFileP = promisify(execFile);

const SKILL_HOOKS_DIR = join(homedir(), ".pi/agent/skills/commit/hooks");

interface HookResult {
  path: string;
  exitCode: number;
  stderr: string;
  stdout: string;
}

async function fileExists(p: string): Promise<boolean> {
  try {
    await access(p);
    return true;
  } catch {
    return false;
  }
}

async function runHook(
  hookPath: string,
  env: Record<string, string>,
): Promise<HookResult> {
  try {
    const { stdout, stderr } = await execFileP("bash", [hookPath], {
      env: { ...process.env, ...env },
      timeout: 120_000, // 2 min timeout
      maxBuffer: 256 * 1024, // 256KB output
    });
    return { path: hookPath, exitCode: 0, stdout, stderr };
  } catch (err: any) {
    return {
      path: hookPath,
      exitCode: err.code ?? 1,
      stdout: err.stdout ?? "",
      stderr: err.stderr ?? err.message ?? "unknown error",
    };
  }
}

async function runPreCommitHooks(
  projectRoot: string,
  commitMessage: string,
): Promise<{ passed: boolean; results: HookResult[] }> {
  const env = {
    COMMIT_MESSAGE: commitMessage,
    VCS: "git",
    PROJECT_ROOT: projectRoot,
  };

  const hooks: string[] = [];
  // Skill hook (always)
  const skillPre = join(SKILL_HOOKS_DIR, "pre-commit");
  if (await fileExists(skillPre)) hooks.push(skillPre);
  // Project hook (if present)
  const projectPre = join(projectRoot, ".pi/hooks/pre-commit");
  if (await fileExists(projectPre)) hooks.push(projectPre);

  const results: HookResult[] = [];
  for (const hookPath of hooks) {
    const r = await runHook(hookPath, env);
    results.push(r);
    if (r.exitCode !== 0) break; // fail-fast
  }

  const passed = results.every((r) => r.exitCode === 0);
  return { passed, results };
}

async function runPostCommitHooks(
  projectRoot: string,
  commitMessage: string,
): Promise<HookResult[]> {
  const env = {
    COMMIT_MESSAGE: commitMessage,
    VCS: "git",
    PROJECT_ROOT: projectRoot,
  };

  const hooks: string[] = [];
  const skillPost = join(SKILL_HOOKS_DIR, "post-commit");
  if (await fileExists(skillPost)) hooks.push(skillPost);
  const projectPost = join(projectRoot, ".pi/hooks/post-commit");
  if (await fileExists(projectPost)) hooks.push(projectPost);

  const results: HookResult[] = [];
  for (const hookPath of hooks) {
    results.push(await runHook(hookPath, env));
  }
  return results;
}

function isCommitCommand(command: string): boolean {
  return /\bgit\s+commit\b/.test(command);
}

function extractMessage(command: string): string {
  // Try to extract -m "message" from the command
  const match = command.match(/-m\s+(?:"([^"]*)"|'([^']*)')/);
  if (match) return match[1] ?? match[2] ?? "";

  // Try heredoc: -m "$(cat <<'EOF'\n...\nEOF\n)"
  const heredoc = command.match(/-m\s+\$\(cat\s+<<'EOF'\s*\n([\s\S]*?)\nEOF\s*\)/);
  if (heredoc) return heredoc[1].trim();

  // Multi-line heredoc variant (used by the SKILL.md)
  const heredoc2 = command.match(/<<'EOF'\s*\n([\s\S]*?)\nEOF/);
  if (heredoc2) return heredoc2[1].trim();

  return "";
}

// Track pending commits to run post-commit hooks on success
const pendingCommits = new Map<string, { message: string; projectRoot: string }>();

export default function (pi: ExtensionAPI) {
  // ── Pre-commit: intercept before commit executes ──────────
  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return undefined;

    const command = event.input.command as string;
    if (!isCommitCommand(command)) return undefined;

    const projectRoot = ctx.cwd;
    const message = extractMessage(command);

    // Run pre-commit hooks
    const { passed, results } = await runPreCommitHooks(projectRoot, message);

    if (!passed) {
      const failed = results.filter((r) => r.exitCode !== 0);
      const reasons = failed.map(
        (r) => `${r.path} (exit ${r.exitCode}): ${r.stderr.slice(-200).trim() || r.stdout.slice(-200).trim()}`,
      );
      const reason = `pre-commit hook(s) failed:\n${reasons.join("\n")}`;
      if (ctx.hasUI) {
        ctx.ui.notify(reason, "error");
      }
      return { block: true, reason };
    }

    // Track for post-commit
    pendingCommits.set(event.toolCallId, { message, projectRoot });
    return undefined; // allow
  });

  // ── Post-commit: run after commit succeeds ───────────────
  pi.on("tool_result", async (event, _ctx) => {
    if (event.toolName !== "bash") return undefined;

    const info = pendingCommits.get(event.toolCallId);
    if (!info) return undefined;

    pendingCommits.delete(event.toolCallId);

    // Only run post-commit if the commit succeeded
    if (event.isError) return undefined;

    // Fire and forget — don't block the agent
    runPostCommitHooks(info.projectRoot, info.message).catch(() => {});
    return undefined;
  });

  // ── Cleanup stale entries on agent end ──────────────────
  pi.on("agent_end", () => {
    pendingCommits.clear();
  });
}
