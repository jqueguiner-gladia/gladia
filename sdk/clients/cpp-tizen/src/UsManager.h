#ifndef _UsManager_H_
#define _UsManager_H_

#include <string>
#include <cstring>
#include <list>
#include <glib.h>
#include "HTTPValidationError.h"
#include "Error.h"

/** \defgroup Operations API Endpoints
 *  Classes containing all the functions for calling API endpoints
 *
 */

namespace Tizen{
namespace ArtikCloud {
/** \addtogroup Us Us
 * \ingroup Operations
 *  @{
 */
class UsManager {
public:
	UsManager();
	virtual ~UsManager();

/*! \brief Read User. *Synchronous*
 *
 * 
 * \param username  *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool readUserImageImageUncolorizationUsersUsernamePostSync(char * accessToken,
	std::string username, 
	void(* handler)(std::string, Error, void* )
	, void* userData);

/*! \brief Read User. *Asynchronous*
 *
 * 
 * \param username  *Required*
 * \param handler The callback function to be invoked on completion. *Required*
 * \param accessToken The Authorization token. *Required*
 * \param userData The user data to be passed to the callback function.
 */
bool readUserImageImageUncolorizationUsersUsernamePostAsync(char * accessToken,
	std::string username, 
	void(* handler)(std::string, Error, void* )
	, void* userData);



	static std::string getBasePath()
	{
		return "http://localhost";
	}
};
/** @}*/

}
}
#endif /* UsManager_H_ */
