ALTER TABLE agent_runs
  DROP CONSTRAINT agent_runs_kind_check;

UPDATE agent_runs
SET kind = 'question'
WHERE kind = 'conversation';

ALTER TABLE agent_runs
  ADD CONSTRAINT agent_runs_kind_check
  CHECK (kind IN ('orientation', 'question'));
