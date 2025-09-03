package com.logicaldoc.cmis;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.math.BigInteger;
import java.sql.SQLException;
import java.util.List;

import org.apache.chemistry.opencmis.commons.definitions.TypeDefinition;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionContainer;
import org.apache.chemistry.opencmis.commons.definitions.TypeDefinitionList;
import org.apache.chemistry.opencmis.commons.enums.BaseTypeId;
import org.apache.chemistry.opencmis.commons.exceptions.CmisInvalidArgumentException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.DocumentTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.FolderTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ItemTypeDefinitionImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.RelationshipTypeDefinitionImpl;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.plugin.PluginException;

public class TypeManagerTest extends AbstractCmisTestCase {

	protected TypeManager testSubject;

	@Before
	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = new TypeManager();
	}

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar", "/logicaldoc-cmis-plugin.jar");
	}

	@Test
	public void testAddType() {
		assertFalse(testSubject.addType(null));

		DocumentTypeDefinitionImpl bad = new DocumentTypeDefinitionImpl();
		bad.setId("my:bad");
		assertFalse(testSubject.addType(bad));

		DocumentTypeDefinitionImpl good = new DocumentTypeDefinitionImpl();
		good.setId("my:doc");
		good.setLocalName("my:doc");
		good.setDisplayName("My Doc");
		good.setBaseTypeId(BaseTypeId.CMIS_DOCUMENT);
		assertTrue(testSubject.addType(good));

		FolderTypeDefinitionImpl f = new FolderTypeDefinitionImpl();
		f.setId("my:folder");
		f.setLocalName("my:folder");
		f.setDisplayName("My Folder");
		f.setBaseTypeId(BaseTypeId.CMIS_FOLDER);
		assertTrue(testSubject.addType(f));

		RelationshipTypeDefinitionImpl r = new RelationshipTypeDefinitionImpl();
		r.setId("my:rel");
		r.setLocalName("my:rel");
		r.setDisplayName("My Relationship");
		r.setBaseTypeId(BaseTypeId.CMIS_RELATIONSHIP);
		assertTrue(testSubject.addType(r));

		ItemTypeDefinitionImpl item = new ItemTypeDefinitionImpl();
		item.setId("my:item");
		item.setLocalName("my:item");
		item.setDisplayName("My Item");
		item.setBaseTypeId(BaseTypeId.CMIS_ITEM);
		assertFalse(testSubject.addType(item));
	}

	@Test
	public void testGetTypesChildren() {
		DocumentTypeDefinitionImpl d1 = new DocumentTypeDefinitionImpl();
		d1.setId("t:doc1");
		d1.setLocalName("t:doc1");
		d1.setDisplayName("Doc1");
		d1.setBaseTypeId(BaseTypeId.CMIS_DOCUMENT);
		assertTrue(testSubject.addType(d1));

		DocumentTypeDefinitionImpl d2 = new DocumentTypeDefinitionImpl();
		d2.setId("t:doc2");
		d2.setLocalName("t:doc2");
		d2.setDisplayName("Doc2");
		d2.setBaseTypeId(BaseTypeId.CMIS_DOCUMENT);
		assertTrue(testSubject.addType(d2));

		TypeDefinitionList roots = testSubject.getTypesChildren(null, null, false, null, null);
		assertEquals(2, roots.getList().size());
		for (TypeDefinition td : roots.getList()) {
			assertTrue(td.getPropertyDefinitions().isEmpty());
		}
		assertEquals(BigInteger.valueOf(2), roots.getNumItems());

		TypeDefinitionList page = testSubject.getTypesChildren(null, "cmis:document", false, BigInteger.ONE,
				BigInteger.ONE);
		assertEquals(0, page.getList().size());
	}

	@Test
	public void testGetTypesDescendants() {
		try {
			testSubject.getTypesDescendants(null, null, BigInteger.ZERO, Boolean.FALSE);
			fail("Expected CmisInvalidArgumentException for depth == 0");
		} catch (CmisInvalidArgumentException expected) {
			/* ok */ }

		List<TypeDefinitionContainer> roots = testSubject.getTypesDescendants(null, null, BigInteger.ONE,
				Boolean.FALSE);
		assertEquals(2, roots.size());
		TypeDefinitionContainer docRoot = null;
		for (var c : roots) {
			if ("cmis:document".equals(c.getTypeDefinition().getId())) {
				docRoot = c;
				break;
			}
		}
		assertNotNull(docRoot);
		assertTrue(docRoot.getTypeDefinition().getPropertyDefinitions().isEmpty());

		List<TypeDefinitionContainer> docTree = testSubject.getTypesDescendants(null, "cmis:document", null,
				Boolean.TRUE);
		assertEquals(1, docTree.size());
		assertFalse(docTree.get(0).getTypeDefinition().getPropertyDefinitions().isEmpty());
	}

	@Test
	public void testGetType() {
		assertNotNull(testSubject.getType("cmis:document"));

		assertNull(testSubject.getType("no:such:type"));
		assertNull(testSubject.getType(null));

		DocumentTypeDefinitionImpl d = new DocumentTypeDefinitionImpl();
		d.setId("my:doc");
		d.setLocalName("my:doc");
		d.setDisplayName("My Doc");
		d.setBaseTypeId(BaseTypeId.CMIS_DOCUMENT);
		assertTrue(testSubject.addType(d));

		TypeDefinition got = testSubject.getType("my:doc");
		assertNotNull(got);
		assertEquals("my:doc", got.getId());
	}

	@Test
	public void testGetTypeDefinition() {
		TypeDefinition doc = testSubject.getTypeDefinition(null, "cmis:document");
		assertNotNull(doc);
		assertEquals("cmis:document", doc.getId());

		try {
			testSubject.getTypeDefinition(null, "no:such:type");
			fail("Expected CmisObjectNotFoundException");
		} catch (CmisObjectNotFoundException expected) {
			/* ok */ }

		try {
			testSubject.getTypeDefinition(null, null);
			fail("Expected CmisObjectNotFoundException for null typeId");
		} catch (CmisObjectNotFoundException expected) {
			/* ok */ }
	}

}