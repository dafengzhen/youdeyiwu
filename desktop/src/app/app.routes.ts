import { Routes } from '@angular/router';
import { HomeComponent } from './pages/home/home.component';
import { UsersComponent } from '@/src/app/pages/users/users.component';
import { PageNotFoundComponent } from '@/src/app/pages/page-not-found/page-not-found.component';
import { UserIdComponent } from '@/src/app/pages/users/user-id/user-id.component';
import { PostIdComponent } from '@/src/app/pages/posts/post-id/post-id.component';

export const routes: Routes = [
  {
    path: '',
    component: HomeComponent,
  },
  {
    path: 'posts/:id',
    component: PostIdComponent,
  },
  {
    path: 'users/:id',
    title: 'My Profile',
    component: UserIdComponent,
  },
  {
    path: 'users',
    title: 'My Profile',
    component: UsersComponent,
  },
  {
    path: '**',
    title: 'Page Not Found',
    component: PageNotFoundComponent,
  },
];
