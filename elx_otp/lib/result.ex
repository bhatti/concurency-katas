defmodule Result do
  @enforce_keys [:url, :status, :started_at]
  defstruct [:url, :status, :started_at, :completed_at, :error]

  def new(req) do
    %Result{url: req.url, status: :started, started_at: System.system_time(:millisecond)}
  end

  def completed(res) do
    %Result{res | status: :completed, completed_at: System.system_time(:millisecond)}
  end

  def failed(res, err) do
    %Result{res | status: :failed, completed_at: System.system_time(:millisecond), error: err}
  end

end
