package com.logicaldoc.webservice.soap.endpoint;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSAttributeOption;
import com.logicaldoc.webservice.model.WSTemplate;

import junit.framework.Assert;

/**
 * Test case for <code>SoapDocumentMetadataService</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class DocumentMetadataServiceTest extends AbstractWebserviceTestCase {
	private TemplateDAO templateDao;

	// Instance under test
	private SoapSecurityService securityServiceImpl;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		templateDao = (TemplateDAO) context.getBean("TemplateDAO");

		// Make sure that this is a SoapSecurityService instance
		securityServiceImpl = new SoapSecurityService();
		securityServiceImpl.setValidateSession(false);
	}

	@Test
	public void testListTemplates() throws Exception {
		SoapDocumentMetadataService docMetadataServiceImpl = new SoapDocumentMetadataService();
		docMetadataServiceImpl.setValidateSession(false);

		List<WSTemplate> templates = docMetadataServiceImpl.listTemplates("");
		Assert.assertNotNull(templates);
		Assert.assertEquals(3, templates.size());
		Assert.assertEquals(-1, templates.get(0).getId());
		Assert.assertEquals(2, templates.get(2).getId());
		Assert.assertEquals("test2_desc", templates.get(2).getDescription());
	}

	@Test
	public void testStoreTemplate() throws Exception {
		SoapDocumentMetadataService docMetadataServiceImpl = new SoapDocumentMetadataService();
		docMetadataServiceImpl.setValidateSession(false);

		WSTemplate wsTemplateTest = new WSTemplate();
		wsTemplateTest.setName("template test");
		wsTemplateTest.setDescription("template test descr");

		Long templateId = docMetadataServiceImpl.storeTemplate("", wsTemplateTest);
		Assert.assertNotNull(templateId);

		Template createdTemplate = templateDao.findById(templateId);
		Assert.assertNotNull(createdTemplate);
		Assert.assertEquals("template test", createdTemplate.getName());
		Assert.assertEquals("template test descr", createdTemplate.getDescription());
	}

	@Test
	public void testDeleteTemplate() throws Exception {
		SoapDocumentMetadataService docMetadataServiceImpl = new SoapDocumentMetadataService();
		docMetadataServiceImpl.setValidateSession(false);

		Template template = templateDao.findById(1);
		Assert.assertNotNull(template);
		docMetadataServiceImpl.deleteTemplate("", template.getId());
		template = templateDao.findById(1);
		Assert.assertNull(template);
	}

	@Test
	public void testSetOptions() throws Exception {
		SoapDocumentMetadataService docMetadataServiceImpl = new SoapDocumentMetadataService();
		docMetadataServiceImpl.setValidateSession(false);

		docMetadataServiceImpl.setAttributeOptions("xxx", 1L, "test", List.of(new WSAttributeOption("val1", null),
				new WSAttributeOption("val2", null), new WSAttributeOption("val3", null)));
		List<String> values = docMetadataServiceImpl.getAttributeOptions("xxx", 1L, "test");
		Assert.assertEquals(3, values.size());

		docMetadataServiceImpl.setAttributeOptions("xxx", 1L, "test",
				List.of(new WSAttributeOption("val1", null), new WSAttributeOption("val2", null),
						new WSAttributeOption("val3", null), new WSAttributeOption("val4", null)));
		values = docMetadataServiceImpl.getAttributeOptions("xxx", 1L, "test");
		Assert.assertEquals(4, values.size());

		docMetadataServiceImpl.setAttributeOptions("xxx", 1L, "test", List.of(new WSAttributeOption("val1", null),
				new WSAttributeOption("val2", null), new WSAttributeOption("val4", null)));
		values = docMetadataServiceImpl.getAttributeOptions("xxx", 1L, "test");
		Assert.assertEquals(3, values.size());
		Assert.assertFalse(Arrays.asList(values).contains("val3"));
	}
}