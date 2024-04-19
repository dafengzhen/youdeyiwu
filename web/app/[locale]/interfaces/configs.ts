export interface IJwtConfig {
  secret: string;
}

export interface IPostConfig {
  createGuide: string;
}

export interface IPointConfig {
  enable: boolean;
  initPoints: number;
}

export interface IRootConfig {
  disableRegistration: boolean;
  disableAnonymousPosts: boolean;
  disableAnonymousComments: boolean;
  disableAnonymousReplies: boolean;
}
