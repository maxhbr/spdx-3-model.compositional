defmodule SPDX3.ModelTest do
  use ExUnit.Case
  doctest SPDX3.Model

  test "exmaple should have expected name" do
    element = SPDX3.Model.example()
    IO.inspect(element)
    name = SPDX3.Model.Element.name(element)
    assert name == "example"
  end
end
