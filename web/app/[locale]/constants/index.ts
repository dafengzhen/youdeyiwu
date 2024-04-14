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

export const ACCEPT_LANGUAGE = 'Accept-Language';

export const NEXT_LOCALE = 'NEXT_LOCALE';

export const JSON_HEADER = {
  'Content-Type': 'application/json',
};

export const BLUR_DATA_URL =
  'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8v+9JPQAIhwMirBSLJQAAAABJRU5ErkJggg==';

export const ACTION_PAGES_DATA = {
  Dashboard: [],
  Sections: [
    'Create',
    'Update',
    'Update Create Post Guide',
    'Update States',
    'Update Admins',
    'Update Tags',
    'Update Tag Groups',
    'Delete',
  ],
  'Section Groups': ['Create', 'Update', 'Update Sections', 'Delete'],
  Tags: ['Create', 'Update', 'Delete'],
  'Tag Groups': ['Create', 'Update', 'Update Tags', 'Delete'],
  Posts: [
    'Update States',
    'Update Tags',
    'Update Section',
    'Delete',
    'Disable Comment Reply',
    'Update User Relationship',
  ],
  'Post Review Queues': ['Receive', 'Return', 'Approved', 'NotApproved'],
  Comments: ['Update State'],
  Messages: ['Create'],
  'Point Rules': ['Update'],
  'Point Permissions': ['Update'],
  Files: ['Delete'],
  Users: [
    'Create',
    'Update States',
    'Update Roles',
    'Delete',
    'Disable Comment Reply',
  ],
  Roles: ['Create', 'Update', 'Update Permissions', 'Delete'],
  Permissions: ['Create', 'Update', 'Update Roles', 'Delete'],
  Menus: ['Create', 'Update', 'Update Roles', 'Delete'],
  Submenus: ['Create', 'Update', 'Update Roles', 'Delete'],
  Actions: ['Create', 'Update', 'Update Roles', 'Delete'],
  JwtConfigs: ['Update'],
  PointConfigs: ['Update'],
  PostConfigs: ['Update Create Guide'],
};
