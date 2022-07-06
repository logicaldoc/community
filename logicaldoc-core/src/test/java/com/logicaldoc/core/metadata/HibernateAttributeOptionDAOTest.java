package com.logicaldoc.core.metadata;

import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;

/**
 * Test case for <code>HibernateAttributeOptionDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1
 */
public class HibernateAttributeOptionDAOTest extends AbstractCoreTCase {

	// Instance under test
	private AttributeOptionDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateAttributeOptionDAO
		dao = (AttributeOptionDAO) context.getBean("AttributeOptionDAO");
	}

	@Test
	public void testDeleteByTemplateIdAndAttribute() {
		List<AttributeOption> options = dao.findBySetIdAndAttribute(1L, null);
		Assert.assertEquals(3, options.size());

		dao.deleteBySetIdAndAttribute(1L, "att1");
		options = dao.findBySetIdAndAttribute(1L, null);
		Assert.assertEquals(1, options.size());

		dao.deleteBySetIdAndAttribute(1L, null);
		options = dao.findBySetIdAndAttribute(1L, null);
		Assert.assertEquals(0, options.size());
	}

	@Test
	public void testDeleteOrphaned() {
		List<AttributeOption> options = dao.findBySetIdAndAttribute(1L, "att1");
		Assert.assertEquals(2, options.size());

		dao.deleteOrphaned(1L, (List<String>)Arrays.asList(new String[]{"pippo","pluto"}));
		options = dao.findBySetIdAndAttribute(1L, "att1");
		Assert.assertEquals(0, options.size());
	}
	
	@Test
	public void testFindByTemplateAndAttribute() {
		List<AttributeOption> options = dao.findBySetIdAndAttribute(1L, "att1");
		Assert.assertEquals(2, options.size());
		options = dao.findBySetIdAndAttribute(1L, null);
		Assert.assertEquals(3, options.size());

		options = dao.findBySetIdAndAttribute(999L, null);
		Assert.assertEquals(0, options.size());

		options = dao.findBySetIdAndAttribute(1L, "att2");
		Assert.assertEquals(1, options.size());
		options = dao.findBySetIdAndAttribute(2L, "att1");
		Assert.assertEquals(1, options.size());
		options = dao.findBySetIdAndAttribute(2L, null);
		Assert.assertEquals(1, options.size());
	}
}