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

defmodule PackBEAM.API do
  @moduledoc """
  A library used to generate an
  <a href="http://github.com/atomvm/AtomVM">AtomVM</a> AVM file from a set of
  files (beam files, previously built AVM files, or even arbitrary data files).

  This is an Elixir interface for the native Erlang packbeam_api library.
  """

  @type path :: path
  @type avm_element :: [atom | {atom, term}]
  @type avm_element_name :: String.t()
  @type options :: %{
          prune: prune :: boolean,
          start_module: start :: atom,
          include_lines: lines :: boolean,
          arch: arch :: atom,
          platform: platform :: atom
        }

  @doc """
  Create an AVM file with default options.

    Equivalent to `create(outpath, inputpaths, defaultopts)`

    where `defaultopts` is `#%{
      :prune => false,
      :start_module => :undefined,
      :application_module => :undefined,
      :include_lines => false,
      :arch => :undefined,
      :platform => :generic_unix
    }`
  """
  @spec create(outpath :: path, inputpaths :: [path]) :: :ok | {:error, reason :: term}
  def create(outpath, inputpaths) do
    :packbeam_api.create(String.to_charlist(outpath), String.to_charlist(inputpaths))
  end

  @doc """
  Create an AVM file.

  This function will create an AVM file at the location specified in `outpath`, using the input
  files specified in `inputpaths` using the specified `options`.
  """
  @spec create(outpath :: path, inputpaths :: [path], options :: options) ::
          :ok | {:error, reason :: term}
  def create(outpath, inputpaths, options) do
    :packbeam_api.create(String.to_charlist(outpath), inputpaths, options)
  end

  @doc """
  List the contents of an AVM.
  """
  @spec list(inputpath :: path) :: [avm_element]
  def list(inputpath) do
    :packbeam_api.list(String.to_charlist(inputpath))
  end

  @doc """
  Extract all or selected elements from an AVM file.

  This function will extract elements of an AVM file at the location specified in `inputpath`,
  specified by the supplied list of names.  The elements from the input AVM file will be written
  into the specified output directory, creating any subdirectories if the AVM file elements contain
  path information.
  """
  @spec extract(inputpath :: path, amvelements :: [avm_element_name], outdir :: path) ::
          :ok | {:error, reason :: term}
  def extract(inputpath, amvelements, outdir) do
    elements = List.foldl(amvelements, [], fn x, acc -> [String.to_charlist(x) | acc] end)
    :packbeam_api.extract(inputpath, elements, outdir)
  end

  @doc """
  Delete selected elements of an AVM file.

  This function will delete elements of an AVM file at the location specified in `inputpath`,
  specified by the supplied list of names.  The output AVM file is written to `outpath`, which may
  be the same as `inputpath`.
  """
  @spec delete(outpath :: path, inputpath :: path, amvelements :: [avm_element_name]) ::
          :ok | {:error, reason :: term}
  def delete(outpath, inputpath, amvelements) do
    elements = List.foldl(amvelements, [], fn x, acc -> [String.to_charlist(x) | acc] end)
    :packbeam_api.delete(String.to_charlist(outpath), inputpath, String.to_charlist(elements))
  end

  @doc """
  Return the name of the element.
  """
  @spec get_element_name(amvelement :: avm_element) :: atom
  def get_element_name(amvelement) do
    :packbeam_api.get_element_name(String.to_charlist(amvelement))
  end

  @doc """
  Return the AVM element data.
  """
  @spec get_element_data(amvelement :: avm_element) :: binary
  def get_element_data(amvelement) do
    :packbeam_api.get_element_data(String.to_charlist(amvelement))
  end

  @doc """
  Return AVM element module, if the element is a BEAM file.
  """
  @spec get_element_module(amvelement :: avm_element) :: module | :undefined
  def get_element_module(amvelement) do
    :packbeam_api.get_element_module(String.to_charlist(amvelement))
  end

  @doc """
  Indicates whether the AVM file element is an entrypoint.
  """
  @spec is_entrypoint(amvelement :: avm_element) :: boolean
  def is_entrypoint(amvelement) do
    :packbeam_api.is_entrypoint(String.to_charlist(amvelement))
  end

  @doc """
  Indicates whether the AVM file element is a BEAM file.
  """
  @spec is_beam(amvelement :: avm_element) :: boolean
  def is_beam(amvelement) do
    :packbeam_api.is_beam(String.to_charlist(amvelement))
  end
end
