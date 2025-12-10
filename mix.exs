#
# This file is part of atomvm_packbeam.
#
# Copyright 2025 Winford (Uncle Grumpy) <winford@object.stream>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule Packbeam.MixProject do
  use Mix.Project

  def project do
    [
      app: :packbeam,
      version: "0.7.5",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Docs
      name: "Packbeam",
      source_url: "https://github.com/atomvm/atomvm_packbeam",
      homepage_url: "https://www.atomvm.org/",
      docs: [
        # The main page in the docs
        main: "README.md",
        skip_undefined_reference_warnings_on: "README.md",
        api_reference: true,
        output: "elixir_docs",
        extras: [
          "README.md",
          "CHANGELOG.md",
          "UPDATING.md",
          "LICENSE",
          "CONTRIBUTING.md",
          "CODE_OF_CONDUCT.md"
        ]
      ],

      # Tests
      elixirc_paths: elixirc_paths(Mix.env())
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # TODO: add property tests
      # {:propcheck, "~> 1.4", only: [:test, :dev]}
    ]
  end
end
