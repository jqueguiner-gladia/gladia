import {interfaces} from "inversify";

import { DefaultService } from './api/default.service';
import { UsService } from './api/us.service';
import { UsersService } from './api/users.service';

export class ApiServiceBinder {
    public static with(container: interfaces.Container) {
        container.bind<DefaultService>("DefaultService").to(DefaultService).inSingletonScope();
        container.bind<UsService>("UsService").to(UsService).inSingletonScope();
        container.bind<UsersService>("UsersService").to(UsersService).inSingletonScope();
    }
}
