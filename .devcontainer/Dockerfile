ARG VERSION="6.0"
FROM mcr.microsoft.com/dotnet/sdk:${VERSION}

RUN useradd -m -s $(which bash) vscode

RUN mkdir -p /workspaces/AdventOfCode2021 && chown vscode:vscode /workspaces/AdventOfCode2021
USER vscode