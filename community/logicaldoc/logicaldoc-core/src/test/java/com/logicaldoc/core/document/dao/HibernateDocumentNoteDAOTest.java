package com.logicaldoc.core.document.dao;

import java.util.List;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.document.DocumentNote;

/**
 * Test case for document note DAO
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class HibernateDocumentNoteDAOTest extends AbstractCoreTCase {

	private DocumentNoteDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentNoteDAO
		dao = (DocumentNoteDAO) context.getBean("DocumentNoteDAO");
	}

	@Test
	public void testFindByDocId() {
		List<DocumentNote> notes = dao.findByDocId(1L);
		Assert.assertNotNull(notes);
		Assert.assertEquals(2, notes.size());
		DocumentNote note = notes.get(0);
		Assert.assertEquals("message for note 1", note.getMessage());
	}

	@Test
	public void testDeleteContentAnnotations() {
		Assert.assertNotNull(dao.findById(2L));
		dao.deleteContentAnnotations(1L);
		Assert.assertNull(dao.findById(2L));
	}
}
