# Testing WP/Region

Use `./fc.sh -h|--help` to visualize the output before commiting changes.

# Recommanded workflow

With default configuration, put a single 'job' function in each test file.
Then:

1. Run `./fc.sh test.i -r` to visualize the region graph and check the proofs
2. Run `./fc.sh test.i -u` to update the region-graph oracle (creates also the oracle directories)
3. Run `./fc.sh test.i -t` to check test is OK (eventually use `-t -show` or `-t -update`)
4. Run `./fc.sh test.i -q` to check qualif test is OK
