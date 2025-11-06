package com.logicaldoc.core.security.authentication;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link DefaultAuthenticator}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
public class DefaultAuthenticatorTest extends AbstractCoreTestCase {

	private DefaultAuthenticator testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = Context.get(DefaultAuthenticator.class);
	}

	@Test
	public void testAuthenticate() throws PersistenceException {
		User user = testSubject.authenticate("admin", "admin");
		assertNotNull(user);

		// Now add a legal not yet confirmed
		UserDAO.get().jdbcUpdate(
				"insert into ld_legal(ld_name, ld_title, ld_date, ld_content) values ('EULA 1', 'EULA', CURRENT_TIMESTAMP, 'test')");

		try {
			testSubject.authenticate("admin", "admin");
			fail("User atutenticated even if the legals were not confirmed");
		} catch (AuthenticationException e) {
			assertNotNull(e);
		}

		// Now confirm the legal
		UserDAO.get().jdbcUpdate(
				"insert into ld_legal_confirmation(ld_legal, ld_date, ld_username, ld_user) values ('EULA 1', CURRENT_TIMESTAMP, 'admin', 'Administrator')");
		user = testSubject.authenticate("admin", "admin");
		assertNotNull(user);
	}
}