package com.logicaldoc.core.document.dao;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentNote;

import junit.framework.Assert;

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
		List<DocumentNote> notes = dao.findByDocId(1L, null);
		Assert.assertNotNull(notes);
		Assert.assertEquals(2, notes.size());
		DocumentNote note = notes.get(0);
		Assert.assertEquals("message for note 1", note.getMessage());
	}
	
	@Test
	public void testFindByDocIdAndType() {
		List<DocumentNote> notes = dao.findByDocIdAndType(1L, null, "x");
		Assert.assertNotNull(notes);
		Assert.assertEquals(1, notes.size());
		DocumentNote note = notes.get(0);
		Assert.assertEquals("message for note 2", note.getMessage());
	}

	@Test
	public void testCopyAnnotations() throws PersistenceException {
		Assert.assertTrue(dao.findByDocId(4L, "2.0").isEmpty());
		dao.copyAnnotations(4L, "1.0", "2.0");
		Assert.assertEquals(1, dao.findByDocId(4L, "2.0").size());
	}
}