import * as api from './api/api';
import * as angular from 'angular';

const apiModule = angular.module('api', [])
.service('DefaultApi', api.DefaultApi)
.service('UsApi', api.UsApi)
.service('UsersApi', api.UsersApi)

export default apiModule;
