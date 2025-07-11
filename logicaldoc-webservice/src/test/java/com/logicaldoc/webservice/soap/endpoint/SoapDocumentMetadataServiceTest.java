package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSAccessControlEntry;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSAttributeSet;
import com.logicaldoc.webservice.model.WSTemplate;

/**
 * Test case for <code>SoapDocumentMetadataService</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class SoapDocumentMetadataServiceTest extends AbstractWebserviceTestCase {
	private TemplateDAO templateDao;

	private SoapDocumentMetadataService testSubject;

	private SoapSecurityService securityServiceImpl;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		templateDao = Context.get(TemplateDAO.class);

		// Make sure that this is a SoapSecurityService instance
		securityServiceImpl = new SoapSecurityService();
		securityServiceImpl.setValidateSession(false);

		testSubject = new SoapDocumentMetadataService();
		testSubject.setValidateSession(false);
	}

	@Test
	public void testListTemplates() throws Exception {
		List<WSTemplate> templates = testSubject.listTemplates("");
		assertNotNull(templates);
		assertEquals(3, templates.size());
		assertEquals(-1, templates.get(0).getId());
		assertEquals(2, templates.get(2).getId());
		assertEquals("test2_desc", templates.get(2).getDescription());
	}

	@Test
	public void testGetTemplate() throws Exception {
		WSTemplate template = testSubject.getTemplate("", "default");
		assertNotNull(template);
		assertEquals("default", template.getName());

		assertNull(testSubject.getTemplate("", "unexisting"));
	}

	@Test
	public void testGetTemplateById() throws Exception {
		WSTemplate template = testSubject.getTemplateById("", -1L);
		assertNotNull(template);
		assertEquals("default", template.getName());

		assertNull(testSubject.getTemplateById("", 999L));
	}

	@Test
	public void testStoreTemplate() throws Exception {
		WSTemplate wsTemplateTest = new WSTemplate();
		wsTemplateTest.setName("template test");
		wsTemplateTest.setDescription("template test descr");

		Long templateId = testSubject.storeTemplate("", wsTemplateTest);
		assertNotNull(templateId);

		Template createdTemplate = templateDao.findById(templateId);
		assertNotNull(createdTemplate);
		assertEquals("template test", createdTemplate.getName());
		assertEquals("template test descr", createdTemplate.getDescription());

		WSTemplate template = testSubject.getTemplateById("", -1L);
		assertNull(template.getAttribute("source").getStringValue());

		template.getAttribute("source").setStringValue("val1");
		testSubject.storeTemplate("", template);

		template = testSubject.getTemplateById("", -1L);
		assertEquals("val1", template.getAttribute("source").getStringValue());
	}

	@Test
	public void testDeleteTemplate() throws Exception {
		Template template = templateDao.findById(1);
		assertNotNull(template);
		testSubject.deleteTemplate("", template.getId());
		template = templateDao.findById(1);
		assertNull(template);
	}

	@Test
	public void testSetOptions() throws Exception {
		testSubject.setAttributeOptions("xxx", 1L, "test", List.of(new WSAttributeOption("val1", null),
				new WSAttributeOption("val2", null), new WSAttributeOption("val3", null)));
		List<String> values = testSubject.getAttributeOptions("xxx", 1L, "test");
		assertEquals(3, values.size());

		testSubject.setAttributeOptions("xxx", 1L, "test",
				List.of(new WSAttributeOption("val1", null), new WSAttributeOption("val2", null),
						new WSAttributeOption("val3", null), new WSAttributeOption("val4", null)));
		values = testSubject.getAttributeOptions("xxx", 1L, "test");
		assertEquals(4, values.size());

		testSubject.setAttributeOptions("xxx", 1L, "test", List.of(new WSAttributeOption("val1", null),
				new WSAttributeOption("val2", null), new WSAttributeOption("val4", null)));
		values = testSubject.getAttributeOptions("xxx", 1L, "test");
		assertEquals(3, values.size());
		assertFalse(values.contains("val3"));
	}

	@Test
	public void testListAttributeSets() throws Exception {
		List<WSAttributeSet> sets = testSubject.listAttributeSets("");
		assertNotNull(sets);
		assertEquals(1, sets.size());
	}

	@Test
	public void testGetAttributeSetById() throws Exception {
		WSAttributeSet set = testSubject.getAttributeSetById("", -1L);
		assertNotNull(set);
		assertEquals("default", set.getName());

		assertNull(testSubject.getAttributeSetById("", 999L));
	}

	@Test
	public void testGetAttributeSet() throws Exception {
		WSAttributeSet set = testSubject.getAttributeSet("", "default");
		assertNotNull(set);
		assertEquals("default", set.getName());

		assertNull(testSubject.getAttributeSet("", "unexisting"));
	}

	@Test
	public void testStoreAttributeSet() throws Exception {
		WSAttributeSet set = testSubject.getAttributeSet("", "default");
		assertNotNull(set);
		assertNull(set.getAttribute("source").getStringValue());
		set.getAttribute("source").setStringValue("val1");

		testSubject.storeAttributeSet("", set);

		set = testSubject.getAttributeSet("", "default");
		assertEquals("val1", set.getAttribute("source").getStringValue());
	}

	@Test
	public void testDeleteAttributeSet() throws Exception {
		WSAttributeSet set = testSubject.getAttributeSetById("", -1L);
		assertNotNull(set);
		assertTrue(testSubject.isReadable("", -1L));
		assertTrue(testSubject.isWritable("", -1L));

		testSubject.deleteAttributeSet("", -1L);

		assertNull(testSubject.getAttributeSetById("", -1L));
	}
	
	@Test
	public void testGetAccessControlList() throws Exception {
		List<WSAccessControlEntry> acl = testSubject.getAccessControlList("", -1);
		assertEquals(2, acl.size());
		
		WSAccessControlEntry ace=new WSAccessControlEntry();
		ace.setGroupId(-4);
		ace.setRead(1);
		acl.add(ace);
		
		testSubject.setAccessControlList("", -1, acl);
		
		acl = testSubject.getAccessControlList("", -1);
		assertEquals(3, acl.size());
	}
	
	@Test
	public void testGetAttributeOptionsByCategory() throws Exception {
		List<String> options = testSubject.getAttributeOptions("", -1, "multi");
		assertEquals(3, options.size());
		
		options = testSubject.getAttributeOptions("", -1, "unexisting");
		assertEquals(0, options.size());
		
		List<WSAttributeOption> opts = testSubject.getAttributeOptionsByCategory("", -1, "multi", "country");
		assertEquals(2, opts.size());
		
		opts = testSubject.getAttributeOptionsByCategory("", -1, "multi", "city");
		assertEquals(1, opts.size());
		
		opts = testSubject.getAttributeOptionsByCategory("", -1, "multi", "unexisting");
		assertEquals(0, opts.size());
		
		opts = testSubject.getAttributeOptionsByCategory("", -1, "unexisting", "country");
		assertEquals(0, opts.size());
	}
}