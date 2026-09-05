const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");

const javascript = fs.readFileSync(process.argv[2], "utf8").replace(/\r\n/g, "\n");
const source = javascript.match(
  /  let readerTimezone = [^\n]+;\n\n  function syncReaderTimezone\(.*?\n  }\n/s
);
assert.ok(source, "The browser time zone reporter must be present");

function reporter(intl) {
  const values = [];
  const context = vm.createContext({
    Intl: intl,
    connectionState: "connected",
    window: {
      Shiny: {
        setInputValue(name, value, options) {
          assert.equal(name, "reader_timezone");
          assert.equal(options.priority, "event");
          values.push(value);
        }
      }
    }
  });
  vm.runInContext(source[0], context);
  return { context, values, sync: context.syncReaderTimezone };
}

function zoneIntl(timeZone) {
  return { DateTimeFormat: () => ({ resolvedOptions: () => ({ timeZone }) }) };
}

for (const intl of [
  undefined,
  zoneIntl(undefined),
  zoneIntl(""),
  { DateTimeFormat: () => { throw new Error("Unavailable"); } }
]) {
  const { sync, values } = reporter(intl);
  sync(true);
  sync();
  assert.deepEqual(values, [""], "Unknown zones must not masquerade as UTC");
  sync(true);
  assert.deepEqual(values, ["", ""], "Reconnect must resend an unknown zone");
}

for (const zone of ["UTC", "America/Detroit"]) {
  const { context, sync, values } = reporter(zoneIntl(zone));
  sync(true);
  sync();
  assert.deepEqual(values, [zone], "Unchanged zones should not be resent");
  context.Intl = zoneIntl("");
  sync();
  context.Intl = zoneIntl(zone);
  sync();
  sync(true);
  assert.deepEqual(values, [zone, "", zone, zone]);
  context.connectionState = "disconnected";
  sync(true);
  assert.equal(values.length, 4, "Disconnected sessions must not send inputs");
}
