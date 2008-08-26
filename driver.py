#! /usr/bin/env python
#
# The main driver starts up with an initial environment and reads line
# after line from standard input. For every line, an external hook is
# executed that's being passed the entire environment that may change
# this environment by returning changes on standard output.
#

import os, sys, logging, subprocess

log = logging.getLogger("driver")

def run_hook():
  log.debug("pre-action env = %r" , os.environ)
  ph = subprocess.Popen( os.environ["STATE"]
                       , stdout = subprocess.PIPE
                       , env = os.environ
                       )
  try:
    resp = read_response(ph.stdout)
  finally:
    ph.stdout.close()
  assert ph.wait() == 0
  log.debug("post-action env = %r", os.environ)
  return resp

def read_response(handle):
  line = handle.readline()
  while line != "":
    log.debug("child wrote %r", line)
    if line == "\n":
      return handle.readlines()
    (key,val) = line.rstrip().split('=', 1)
    if val != "":
      os.environ[key] = val
    else:
      del os.environ[key]
    line = handle.readline()
  else:
    return []

def main(input_handle = sys.stdin, output_handle = sys.stdout):
  log.info("initial environment: %r",  os.environ)
  line = input_handle.readline()
  while line != "":
    os.environ["line"] = line.rstrip()
    retries = 0
    resp = run_hook()
    while resp == []:
      retries += 1
      assert retries < 10
      resp = run_hook()
    output_handle.writelines(resp)
    line = input_handle.readline()

if __name__ == "__main__":
  logging.basicConfig(level = 10, format = "%(levelname)s: %(message)s")
  os.environ = { "STATE" : "noop" }
  main()
