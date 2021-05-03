export * from './default.service';
import { DefaultService } from './default.service';
export * from './us.service';
import { UsService } from './us.service';
export * from './users.service';
import { UsersService } from './users.service';
export const APIS = [DefaultService, UsService, UsersService];
