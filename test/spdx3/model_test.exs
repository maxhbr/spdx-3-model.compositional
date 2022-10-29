defmodule SPDX3.ModelTest do
  use ExUnit.Case
  doctest SPDX3.Model

  test "greets the world" do
    assert SPDX3.Model.hello() == :world
  end
end
