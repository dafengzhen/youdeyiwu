import { obtainCredentials } from '@/app/common/server';

export const GET = 'GET';
export const POST = 'POST';
export const PUT = 'PUT';
export const PATCH = 'PATCH';
export const DELETE = 'DELETE';

export const TK = '_youdeyiwu_tk';

export const SECURE_TK = '__Secure_youdeyiwu_tk';

export const AUTHORIZATION = 'Authorization';

export const BEARER = 'Bearer';

export const LOCATION = 'Location';

export const JSON_HEADER = {
  'Content-Type': 'application/json',
};

export const AUTHENTICATION_HEADER = (tk?: string): Record<string, string> => {
  const _tk = tk ?? obtainCredentials();

  if (_tk) {
    return {
      [AUTHORIZATION]: `${BEARER} ${_tk}`,
    };
  }

  return {};
};

export const TEST_AUTHENTICATION_HEADER = (tk = '') => {
  return {
    [AUTHORIZATION]: tk,
  };
};

export const ACTION_PAGES_DATA = {
  Dashboard: [],
  Sections: [
    'Create',
    'Update',
    'Update States',
    'Update Admins',
    'Update Tags',
    'Update Tag Groups',
    'Delete',
  ],
  'Section Groups': ['Create', 'Update', 'Update Sections', 'Delete'],
  Tags: ['Create', 'Update', 'Delete'],
  'Tag Groups': ['Create', 'Update', 'Update Tags', 'Delete'],
  Posts: ['Update States', 'Update Tags', 'Update Section'],
  'Post Review Queues': ['Receive', 'Return', 'Approved', 'NotApproved'],
  Comments: [],
  Messages: ['Create'],
  'Point Rules': ['Update'],
  'Point Permissions': ['Update'],
  Users: ['Create', 'Update States', 'Update Roles', 'Delete'],
  Roles: ['Create', 'Update', 'Update Permissions', 'Delete'],
  Permissions: ['Create', 'Update', 'Update Roles', 'Delete'],
  Menus: ['Create', 'Update', 'Update Roles', 'Delete'],
  Submenus: ['Create', 'Update', 'Update Roles', 'Delete'],
  Actions: ['Create', 'Update', 'Update Role', 'Delete'],
  JwtConfigs: ['Update'],
  PointConfigs: ['Update'],
};
