package com.logicaldoc.core.security.authentication;

import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.apikey.ApiKey;
import com.logicaldoc.core.security.apikey.ApiKeyDAO;
import com.logicaldoc.core.security.user.User;

import jakarta.annotation.Resource;

/**
 * This authenticator uses the API Key
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
@Component("apiKeyAuthenticator")
public class ApiKeyAuthenticator extends DefaultAuthenticator {

	@Resource(name = "apiKeyDAO")
	protected ApiKeyDAO apiKeyDAO;

	@Override
	public User authenticate(String username, String password) throws AuthenticationException {
		// Cannot authenticate without an API Key
		return null;
	}

	@Override
	public User authenticate(String username, String password, String apikey, Client client)
			throws AuthenticationException {
		// Skip whatever key that is not an API Key
		if (StringUtils.isEmpty(apikey) || !apikey.startsWith("ld-"))
			return null;

		User user;
		try {
			ApiKey apiKey = apiKeyDAO.findByKey(apikey);
			if (apiKey == null)
				throw new AuthenticationException("Unexisting ApiKey " + StringUtils.abbreviate(apikey, 10));
			user = userDAO.findById(apiKey.getUserId());
			if (user == null)
				throw new AuthenticationException("Unexisting User " + apiKey.getUserId());

			// Update the last used
			apiKeyDAO.jdbcUpdate("update ld_apikey set ld_lastused = :now where ld_id = :id",
					Map.of("now", new Date(), "id", apiKey.getId()));
		} catch (PersistenceException | NoSuchAlgorithmException e) {
			throw new AuthenticationException(this, "dataerror", e);
		}

		validateUser(user);

		return user;
	}
}