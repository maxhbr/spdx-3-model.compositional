defmodule SPDX3.Model do
  @moduledoc """
  Documentation for `SPDX3.Model`.
  """

  defmodule Element do
    defstruct a: 1, b: "two"
  end

  @doc """
  Hello world.

  ## Examples

      iex> SPDX3.Model.hello()
      :world

  """
  def hello do
    :world
  end
end
