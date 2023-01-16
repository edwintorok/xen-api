#!/bin/sh
set -eu
ENV="DOCKER_BUILDKIT=1 BUILDKIT_PROGRESS=plain PROGRESS_NO_TRUNC=1"
COMMAND="command"
CLI=$("${COMMAND}" -v docker podman | head -n1)
if [ -z "${CLI}" ]; then
    HOSTEXEC=$(${COMMAND} -v distrobox-host-exec)
    CLI="${HOSTEXEC} env ${ENV} $($HOSTEXEC sh -c 'command -v docker podman' | head -n1)"
fi

JOBS=
case "${CLI}" in
    *"podman"*)
        # Buildkit would automatically build in parallel, podman doesn't
        JOBS=" --jobs $(nproc)"
        # FIXME: this can only be added to build but not run commands
        ;;
esac
echo "export ${ENV}"
echo "exec ${CLI} \"\$@\""
