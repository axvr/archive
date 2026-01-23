defmodule Uk.Axvr.Amb do
  def fail!, do: throw(:amb_fail)
  def cut!, do: throw(:amb_cut)

  defmacro assert!(pred) do
    quote do
      if !unquote(pred), do: Uk.Axvr.Amb.fail!()
    end
  end

  defp wrap(func) do
    fn choice ->
      try do
        {:ok, func.(choice)}
      catch
        :amb_fail -> {:amb_fail, nil}
      end
    end
  end

  def possibilities(choices, func), do: possibilities(choices, func, [])

  def possibilities(choices, func, opts) do
    wrapped = wrap(func)

    possibilities =
      try do
        for choice <- choices, {state, result} = wrapped.(choice), state == :ok,
          do: result
      catch
        :amb_cut -> []
      end

    case possibilities do
      [] -> if Keyword.get(opts, :fail, true), do: fail!(), else: []
      res -> res
    end
  end

  # TODO: make "choose" short circuit.

  def choose(choices, func), do: choose(choices, func, [])

  def choose(choices, func, opts) do
    [choice | _] = possibilities(choices, func, opts)
    choice
  end
end
