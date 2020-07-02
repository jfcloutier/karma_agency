defmodule AndyKarmaTest do
  use ExUnit.Case
  doctest AndyKarma

  test "greets the world" do
    assert AndyKarma.hello() == :world
  end
end
