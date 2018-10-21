defmodule FS.Mixfile do
  use Mix.Project

  def project do
    [app: :fs,
     version: "4.10.0",
     description: "Erlang File System Listener",
     deps: deps,
     docs: [],
     package: package]
  end

  defp package do
    [name: :fs,
     files: ["include", "priv", "src", "c_src", "LICENSE", "README.md", "rebar.config"],
     maintainers: ["Vladimir Kirillov", "Namdak Tonpa"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/fs"}]
   end

  defp deps do
     [{:ex_doc, ">= 0.0.0", only: :dev}]
  end
end
