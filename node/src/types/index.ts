export enum State {
  Pending,
  Started,
  Completed,
  Failed,
}

export class Request {
  url: string;
  depth: number;
  timeoutMillis: number;
  createdAt: Date;

  constructor(url: string, depth: number, timeoutMillis: number) {
    this.url = url;
    this.depth = depth;
    this.timeoutMillis = timeoutMillis;
    this.createdAt = new Date();
  }
}

export class Response {
  status: State;
  childURLs: number;
  error: string;
  startedAt: Date;
  completedAt: Date;

  constructor() {
    this.status = State.Started;
    this.startedAt = new Date();
    this.childURLs = 0;
    this.error = '';
    this.completedAt = new Date(0);
  }

  succeeded(total: number) {
    this.childURLs = total;
    this.status = State.Completed;
    this.completedAt = new Date();
  }

  failed(error: string) {
    this.error = error;
    this.status = State.Failed;
    this.completedAt = new Date();
  }
}
