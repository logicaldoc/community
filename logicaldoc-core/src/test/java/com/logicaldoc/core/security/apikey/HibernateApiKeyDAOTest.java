package com.logicaldoc.core.security.apikey;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for @link {@link HibernateApiKeyDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
public class HibernateApiKeyDAOTest extends AbstractCoreTestCase {

	private ApiKeyDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateAPIKeyDAO
		testSubject = ApiKeyDAO.get();
	}

	@Test
	public void testDelete() throws PersistenceException {
		assertNotNull(testSubject.findById(apiKey.getId()));
		testSubject.delete(apiKey.getId());
		assertNull(testSubject.findById(apiKey.getId()));
	}

	@Test
	public void testFindByName() throws PersistenceException {
		assertNotNull(testSubject.findByName(apiKey.getName(), 1L));
		assertNull(testSubject.findByName(apiKey.getName(), 2L));
		assertNull(testSubject.findByName("xx", 1L));
	}

	@Test
	public void testFindByKey() throws PersistenceException, NoSuchAlgorithmException {
		assertNotNull(testSubject.findByKey(apiKey.getDecodedKey()));
		assertNull(testSubject.findByKey("test"));
	}

	@Test
	public void testFindByUser() throws PersistenceException {
		List<ApiKey> keys = testSubject.findByUser(1L);
		assertEquals(1, keys.size());
		assertTrue(keys.stream().anyMatch(k -> apiKey.getName().equals(k.getName())));

		ApiKey key2 = new ApiKey();
		key2.setName("test2");
		key2.setUserId(1L);
		testSubject.store(key2);

		keys = testSubject.findByUser(1L);
		assertEquals(2, keys.size());
		assertTrue(keys.stream().anyMatch(k -> apiKey.getName().equals(k.getName())));
		assertTrue(keys.stream().anyMatch(k -> "test2".equals(k.getName())));

		assertTrue(testSubject.findByUser(99L).isEmpty());
	}
}