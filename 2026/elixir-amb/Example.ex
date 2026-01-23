defmodule Uk.Axvr.Example do
  require Uk.Axvr.Amb
  alias Uk.Axvr.Amb

  def do_something() do
    Amb.possibilities([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], fn choice ->
      Amb.assert!(even?(choice))
      choice
    end)
  end

  defp even?(num), do: rem(num, 2) == 0
end
