package com.logicaldoc.core.communication;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateMessageTemplateDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class HibernateMessageTemplateDAOTest extends AbstractCoreTestCase {
	// Instance under test
	private MessageTemplateDAO dao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		// Retrieve the instance under test from spring context.
		// Make sure that it is an HibernateMessageTemplateDAO
		dao = (MessageTemplateDAO) context.getBean("MessageTemplateDAO");
	}

	@Test
	public void testFindByLanguage() throws PersistenceException {
		Collection<MessageTemplate> coll = dao.findByLanguage("en", 1L);
		Assert.assertEquals(9, coll.size());
		coll = dao.findByLanguage("it", 1L);
		Assert.assertEquals(1, coll.size());
		coll = dao.findByLanguage("de", 1L);
		Assert.assertEquals(0, coll.size());
	}
	
	@Test
	public void testFindByName() throws PersistenceException {
		Collection<MessageTemplate> coll = dao.findByName("psw.rec1", 1L);
		Assert.assertEquals(1, coll.size());
		coll = dao.findByName("psw.rec1", 2L);
		Assert.assertEquals(0, coll.size());
		coll = dao.findByName("xxxxx", 1L);
		Assert.assertEquals(0, coll.size());
	}
	
	@Test
	public void testFindByTypeLanguage() throws PersistenceException {
		Collection<MessageTemplate> coll = dao.findByTypeAndLanguage(MessageTemplate.TYPE_SYSTEM,"en", 1L);
		Assert.assertEquals(6, coll.size());
		coll = dao.findByTypeAndLanguage("xxx","en", 1L);
		Assert.assertEquals(0, coll.size());
	}

	@Test
	public void testFindByNameAndLanguage() throws PersistenceException {
		Map<String, Object> dictionary = new HashMap<>();
		dictionary.put("username", "pippo");
		dictionary.put("xxx", "label");

		MessageTemplate tmp = dao.findByNameAndLanguage("test1", "en", 1L);
		Assert.assertNotNull(tmp);
		Assert.assertEquals("test1", tmp.getName());
		Assert.assertEquals("body pippo label", tmp.getFormattedBody(dictionary));
		Assert.assertEquals("subject label", tmp.getFormattedSubject(dictionary));

		tmp = dao.findByNameAndLanguage("test1", "de", 1L);
		Assert.assertNotNull(tmp);
		Assert.assertEquals("test1", tmp.getName());
		Assert.assertEquals("en", tmp.getLanguage());

		tmp = dao.findByNameAndLanguage("test1", "it", 1L);
		Assert.assertNotNull(tmp);
		Assert.assertEquals("test1", tmp.getName());
		Assert.assertEquals("corpo pippo label", tmp.getFormattedBody(dictionary));
		Assert.assertEquals("soggetto label", tmp.getFormattedSubject(dictionary));

		tmp = dao.findByNameAndLanguage("xxxxxx", "en", 1L);
		Assert.assertNull(tmp);
	}
}
