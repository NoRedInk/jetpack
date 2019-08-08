{ pkgs, ormolu }:

# Ormolu is very new and doesn't have support in many editors yet. By exposing
# it under an alias `hindent` we can trick editors into running Ormolu.
pkgs.writeShellScriptBin "hindent" ''
  #!/usr/bin/env bash
  mode="inplace"
  file=()
  args=()

  while [[ $# -gt 0 ]]
  do
  key="$1"

  case $key in
      -h|--help)
      echo "Hi! I'm Ormolu, a fancy new Haskell code formatter."
      echo "Not a lot of editors support me yet, so I'm pretending to be hindent."
      echo "Editors can call me like they would hindent, but secretly I will format the code they pass me."
      echo "If you're not an editor, you should probably call me directly. Run 'ormolu --help' to learn more!"
      exit 0;
      ;;
      --validate)
      mode="check"
      shift
      ;;
      --version) # same in ormolu and hindent
      args+=("$1")
      shift
      ;;
      --line-length|--indent-size|--tab-size|--style|-X)
      # not supported by ormolu
      shift
      shift
      ;;
      --no-force-newline|--sort-imports|--no-sort-imports)
      # not supported by ormolu
      shift
      ;;
      *)
      file+=("$1")
      shift
      ;;
  esac
  done

  # get input from stdin
  if [ ''${#file[@]} -eq 0 ]; then
    tmpfile=$(mktemp)
    cat - > $tmpfile
    file=("$tmpfile")
    mode="stdout"
  fi

  ormolu --mode $mode "''${file[@]}"
''
