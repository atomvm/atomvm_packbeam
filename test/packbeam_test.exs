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

defmodule PackbeamTest do
  use ExUnit.Case
  # doctest PackbeamTest

  test "Pack with prune: false" do
    basedir = "_build/" <> Atom.to_string(Mix.env()) <> "/lib/packbeam/ebin"

    files = [
      "Elixir.A.beam",
      "Elixir.B.beam",
      "Elixir.C.beam",
      "Elixir.D.beam",
      "Elixir.E.beam",
      "Elixir.F.beam"
    ]

    filelist =
      List.foldr(files, [], fn x, acc -> [String.to_charlist(Path.join(basedir, x)) | acc] end)

    avm = "_build/" <> Atom.to_string(Mix.env()) <> "/testpack.avm"
    assert PackBEAM.API.create(avm, filelist, %{prune: false, start_module: A}) == :ok
    assert File.exists?(avm) == true
    elements0 = PackBEAM.API.list(avm)
    [element1, element2, element3, element4, element5, element6 | _elements1] = elements0
    assert List.keyfind(element1, A, 1) == {:module, A}
    assert List.keyfind(element2, B, 1) == {:module, B}
    assert List.keyfind(element3, C, 1) == {:module, C}
    assert List.keyfind(element4, D, 1) == {:module, D}
    assert List.keyfind(element5, E, 1) == {:module, E}
    assert List.keyfind(element6, F, 1) == {:module, F}
    assert List.last(elements0) == element6
  end

  test "Pack with prune: true" do
    basedir = "_build/" <> Atom.to_string(Mix.env()) <> "/lib/packbeam/ebin"

    files = [
      "Elixir.A.beam",
      "Elixir.B.beam",
      "Elixir.C.beam",
      "Elixir.D.beam",
      "Elixir.E.beam",
      "Elixir.F.beam"
    ]

    filelist =
      List.foldr(files, [], fn x, acc -> [String.to_charlist(Path.join(basedir, x)) | acc] end)

    pruned_avm = "_build/" <> Atom.to_string(Mix.env()) <> "/testpack_pruned.avm"

    assert PackBEAM.API.create(pruned_avm, filelist, %{
             prune: true,
             start_module: A
           }) == :ok

    assert File.exists?(pruned_avm) == true
    elements0 = PackBEAM.API.list(pruned_avm)
    [element1, element2, element3, element4, element5 | _elements1] = elements0
    assert List.keyfind(element1, A, 1) == {:module, A}
    assert List.keyfind(element2, B, 1) == {:module, B}
    assert List.keyfind(element3, C, 1) == {:module, C}
    assert List.keyfind(element4, E, 1) == {:module, E}
    assert List.keyfind(element5, F, 1) == {:module, F}
    assert List.last(elements0) == element5
  end
end
