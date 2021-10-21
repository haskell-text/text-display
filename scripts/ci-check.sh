set -euxo pipefail

make lint
make build
make test
