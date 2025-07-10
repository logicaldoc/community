package com.logicaldoc.core.communication;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateMessageTemplateDAO</code> *
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class HibernateMessageTemplateDAOTest extends AbstractCoreTestCase {
	// Instance under test
	private MessageTemplateDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context.
		// Make sure that it is an HibernateMessageTemplateDAO
		dao = Context.get(MessageTemplateDAO.class);
	}

	@Test
	public void testFindByLanguage() throws PersistenceException {
		Collection<MessageTemplate> coll = dao.findByLanguage("en", 1L);
		assertEquals(9, coll.size());
		coll = dao.findByLanguage("it", 1L);
		assertEquals(1, coll.size());
		coll = dao.findByLanguage("de", 1L);
		assertEquals(0, coll.size());
	}

	@Test
	public void testFindByName() throws PersistenceException {
		Collection<MessageTemplate> coll = dao.findByName("psw.rec1", 1L);
		assertEquals(1, coll.size());
		coll = dao.findByName("psw.rec1", 2L);
		assertEquals(0, coll.size());
		coll = dao.findByName("xxxxx", 1L);
		assertEquals(0, coll.size());
	}

	@Test
	public void testFindByTypeLanguage() throws PersistenceException {
		Collection<MessageTemplate> coll = dao.findByTypeAndLanguage(MessageTemplate.TYPE_SYSTEM, "en", 1L);
		assertEquals(6, coll.size());
		coll = dao.findByTypeAndLanguage("xxx", "en", 1L);
		assertEquals(0, coll.size());
	}

	@Test
	public void testFindByNameAndLanguage() throws PersistenceException, AutomationException {
		Map<String, Object> dictionary = new HashMap<>();
		dictionary.put("username", "pippo");
		dictionary.put("xxx", "label");

		MessageTemplate tmp = dao.findByNameAndLanguage("test1", "en", 1L);
		assertNotNull(tmp);
		assertEquals("test1", tmp.getName());
		assertEquals("body pippo label", tmp.getFormattedBody(dictionary));
		assertEquals("subject label", tmp.getFormattedSubject(dictionary));

		tmp = dao.findByNameAndLanguage("test1", "de", 1L);
		assertNotNull(tmp);
		assertEquals("test1", tmp.getName());
		assertEquals("en", tmp.getLanguage());

		tmp = dao.findByNameAndLanguage("test1", "it", 1L);
		assertNotNull(tmp);
		assertEquals("test1", tmp.getName());
		assertEquals("corpo pippo label", tmp.getFormattedBody(dictionary));
		assertEquals("soggetto label", tmp.getFormattedSubject(dictionary));

		tmp = dao.findByNameAndLanguage("xxxxxx", "en", 1L);
		assertNull(tmp);
	}
}
