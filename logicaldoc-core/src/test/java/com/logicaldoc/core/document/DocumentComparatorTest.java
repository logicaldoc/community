package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.document.dao.DocumentDAO;

public class DocumentComparatorTest extends AbstractCoreTestCase {

	// Instance under test
	private DocumentDAO dao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentDAO
		dao = (DocumentDAO) context.getBean("DocumentDAO");
	}

	@Test
	public void testGetComparatorString() {

		Comparator<AbstractDocument> fdatec = DocumentComparator.getComparator("fileName desc, date asc");
		assertNotNull(fdatec);

		// add some document to a list
		List<Document> docs = dao.findAll();

		// check that the list is sorted
		Collections.sort(docs, fdatec);

		Document firstDoc = docs.get(0);
		assertEquals("pluto.pdf", firstDoc.getFileName());

		Document lstDoc = docs.get(3);
		assertEquals("context.xml", lstDoc.getFileName());
	}
}
