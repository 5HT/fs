defmodule FS.Mixfile do
  use Mix.Project

  def project do
    [app: :fs,
     version: "6.1.1",
     description: "FS Native Listener (Mac Windows Linux)",
     deps: deps(),
     docs: [],
     package: package()]
  end

  defp package do
    [name: :fs,
     files: ["include", "priv", "src", "c_src", "LICENSE", "README.md", "rebar.config"],
     maintainers: ["Vladimir Kirillov", "Namdak Tonpa"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/fs"}]
   end

  defp deps do
     [{:ex_doc, "~> 0.11", only: :dev}]
  end
end
