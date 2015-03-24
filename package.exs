defmodule FS.Mixfile do
  use Mix.Project

  def project do
    [app: :fs,
     version: "0.9.0",
     description: "Erlang FileSystem Listener",
     package: package]
  end

  defp package do
    [files: ~w(c_src include priv src LICENSE package.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/fs"}]
   end
end
