package com.logicaldoc.cmis;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.data.Acl;
import org.apache.chemistry.opencmis.commons.data.AllowableActions;
import org.apache.chemistry.opencmis.commons.data.ContentStream;
import org.apache.chemistry.opencmis.commons.data.FailedToDeleteData;
import org.apache.chemistry.opencmis.commons.data.ObjectData;
import org.apache.chemistry.opencmis.commons.data.ObjectInFolderList;
import org.apache.chemistry.opencmis.commons.data.ObjectList;
import org.apache.chemistry.opencmis.commons.data.ObjectParentData;
import org.apache.chemistry.opencmis.commons.data.Properties;
import org.apache.chemistry.opencmis.commons.data.PropertyData;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.ContentStreamImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertiesImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyIdImpl;
import org.apache.chemistry.opencmis.commons.impl.dataobjects.PropertyStringImpl;
import org.apache.chemistry.opencmis.commons.impl.server.ObjectInfoImpl;
import org.apache.chemistry.opencmis.commons.server.ObjectInfo;
import org.apache.chemistry.opencmis.commons.spi.Holder;
import org.java.plugin.PluginLifecycleException;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentHistoryDAO;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderEvent;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.folder.FolderHistoryDAO;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;

public class LDRepositoryTest extends AbstractCmisTestCase {

	private FolderDAO fdao;

	private DocumentDAO ddao;

	protected static Logger log = LoggerFactory.getLogger(LDRepositoryTest.class);

	protected SearchEngine engine;

	protected Session session;

	protected LDRepository testSubject;

	@Before
	@Override
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();

		engine = (SearchEngine) context.getBean("SearchEngine");

		try {
			addHits();
		} catch (Exception e) {
			throw new IOException(e.getMessage(), e);
		}

		fdao = (FolderDAO) context.getBean("FolderDAO");

		ddao = (DocumentDAO) context.getBean("DocumentDAO");

		session = SessionManager.get().newSession("admin", "admin", null);

		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);
		assertNotNull(folder);
		log.info(folder.getName());

		testSubject = new LDRepository(folder, session.getSid());
	}

	@Override
	protected List<String> getPluginArchives() {
		return List.of("/logicaldoc-core-plugin.jar");
	}

	private void addHits() throws Exception {

		Document document = new Document();
		document.setId(1L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		Folder fold = new Folder();
		fold.setId(5);
		fold.setName("root");
		document.setFolder(fold);
		engine.addHit(document, "Questo � un documento di prova. Per fortuna che esistono i test. document");

		// Adding unexisting document 111
		document = new Document();
		document.setId(2L);
		document.setFileName("test.doc");
		document.setTemplateId(0L);
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document,
				"This is another test documents just for test insertion.Solr is an enterprise-ready, Lucene-based search server that supports faceted ... This is useful for retrieving and highlighting the documents contents for display but is not .... hl, When hl=true , highlight snippets in the query response.");

		document = new Document();
		document.setId(3L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document, "Another document");

		document = new Document();
		document.setId(4L);
		document.setFileName("test.doc");
		document.setLanguage("en");
		document.setDate(new Date());
		document.setFolder(fold);
		engine.addHit(document,
				"Lorem ipsum dolor sit amet, consectetur 5568299afbX0 ZKBKCHZZ80A CH8900761016116097873 adipisicing elit");

		document = new Document();
		document.setId(5L);
		document.setFileName("flexspaces.xlsx");
		document.setLanguage("en");
		document.setDate(new Date());
		Folder fold04 = new Folder();
		fold04.setId(4);
		fold04.setName("Default");
		document.setFolder(fold04);
		document.addTag("Google");
		document.addTag("document");
		document.addTag("numbered");
		engine.addHit(document,
				"12, 81390264001300, FLEXSPACE NO 1 LLP, T/A FLEXSPACE, UNIT 13 EVANS BUSINESS CENTRE, VINCENT CAREY ROAD, ROTHERWAS INDUSTRIAL ESTATE, HEREFORD, HR2");
	}

	@Test
	public void testQuery() throws PluginLifecycleException, PersistenceException {
		// Search by filename
		String query = "SELECT cmis:objectId,cmis:name,ldoc:tags FROM cmis:document WHERE cmis:name = 'test.doc'";

		ObjectList ol = testSubject.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(5, ol.getObjects().size());

		// Search by filename
		query = "SELECT cmis:objectId,cmis:name,ldoc:tags FROM cmis:document WHERE cmis:name LIKE 'test%'";
		ol = testSubject.query(query, 40);
		assertEquals(5, ol.getObjects().size());

		// Search by filename
		query = "SELECT cmis:objectId,cmis:name,ldoc:tags FROM cmis:document WHERE cmis:name LIKE '%.doc'";
		ol = testSubject.query(query, 40);
		assertEquals(5, ol.getObjects().size());

		// Search by filename
		query = "SELECT cmis:objectId,cmis:name,ldoc:tags FROM cmis:document WHERE cmis:name LIKE '%.doc'";
		ol = testSubject.query(query, 40);
		assertEquals(5, ol.getObjects().size());

		// Search by filename
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE cmis:name LIKE '%flexspaces%'";
		ol = testSubject.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(1, ol.getObjects().size());

		List<ObjectData> results = ol.getObjects();
		for (Iterator<ObjectData> iterator = results.iterator(); iterator.hasNext();) {
			ObjectData objectData = (ObjectData) iterator.next();

			PropertyData<?> oid = objectData.getProperties().getProperties().get("cmis:objectId");
			PropertyData<?> cmisname = objectData.getProperties().getProperties().get("cmis:name");

			String cmisDocID = (String) oid.getFirstValue();
			assertEquals("doc.5", cmisDocID);
			assertEquals("flexspaces.xlsx", (String) cmisname.getFirstValue());

			// access property directly
			String xcxx = LDRepository.getStringProperty(objectData.getProperties(), "cmis:objectId");
			log.info("cmis:objectId: {}", xcxx);

			String tags = LDRepository.getStringProperty(objectData.getProperties(), "ldoc:tags");
			log.info("ldoc:tags: {}", tags);
		}

		// Search full text on: fileName, title, tags and content
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE CONTAINS('FLEXSPACE')";
		ol = testSubject.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(1, ol.getObjects().size());

		// Search by tag
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:tags = 'document'";
		ol = testSubject.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(1, ol.getObjects().size());

		// Search by language
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:language = 'en'";
		ol = testSubject.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(5, ol.getObjects().size());

		// Search full text on: fileName, title, tags and content with folderId
		// tree filter
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE CONTAINS('FLEXSPACE') in_tree ('fld.6')";
		ol = testSubject.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(1, ol.getObjects().size());

		// Search full text on: content with folderId tree filter
		query = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document WHERE ldoc:content CONTAINS('document') in_tree ('fld.5')";
		ol = testSubject.query(query, 40);
		log.info("found results: {}", ol.getObjects().size());
		assertEquals(3, ol.getObjects().size());
	}

	@Test
	public void testGetObjectInfo() throws PersistenceException {
		ObjectInfo oi = testSubject.getObjectInfo("doc.5", null);
		assertNotNull(oi);
		assertEquals("doc.5", oi.getId());
		assertEquals("flexspaces.xlsx", oi.getFileName());

		oi = testSubject.getObjectInfo("fld.4", null);
		assertNotNull(oi);
		assertEquals("fld.4", oi.getId());
		assertEquals("Default", oi.getName());
	}

	@Test
	public void testUpdateProperties() throws PersistenceException {
		ObjectInfo oi = testSubject.getObjectInfo("doc.5", null);
		assertNotNull(oi);
		assertEquals("doc.5", oi.getId());
		assertEquals("flexspaces.xlsx", oi.getFileName());

		Holder<String> objectId = new Holder<String>("doc.5");

		Properties props = oi.getObject().getProperties();
		log.debug((String) props.getProperties().get("cmis:name").getFirstValue());

		// Update the name of the file
		PropertiesImpl pimp = new PropertiesImpl();

		String pid = "cmis:name";
		PropertyStringImpl p = new PropertyStringImpl(pid, "snow angels.txt");
		p.setQueryName(pid);
		pimp.addProperty(p);

		testSubject.updateProperties(null, objectId, pimp, null);

		oi = testSubject.getObjectInfo("doc.5", null);
		assertNotNull(oi);
		assertEquals("doc.5", oi.getId());
		assertEquals("snow angels.txt", oi.getFileName());

		// now try with a folder
		ObjectInfo oi2 = testSubject.getObjectInfo("fld.6", null);
		assertNotNull(oi2);
		assertEquals("fld.6", oi2.getId());
		assertEquals("folder6", oi2.getName());

		Holder<String> objectId2 = new Holder<String>("fld.6");

		Properties props2 = oi2.getObject().getProperties();
		log.debug((String) props2.getProperties().get("cmis:name").getFirstValue());

		pimp = new PropertiesImpl();
		pid = "cmis:name";
		p = new PropertyStringImpl(pid, "donald.txt");
		p.setQueryName(pid);
		pimp.addProperty(p);

		testSubject.updateProperties(null, objectId2, pimp, null);

		oi = testSubject.getObjectInfo("fld.6", null);
		assertNotNull(oi);
		assertEquals("fld.6", oi.getId());
		assertEquals("donald.txt", oi.getName());
	}

	@Test
	public void testCreateDocument() throws PersistenceException {
		PropertiesImpl props = new PropertiesImpl();

		PropertyIdImpl p = new PropertyIdImpl(PropertyIds.OBJECT_TYPE_ID, "cmis:document");
		p.setQueryName(PropertyIds.OBJECT_TYPE_ID);
		props.addProperty(p);

		p = new PropertyIdImpl(PropertyIds.NAME, "pippo.txt");
		p.setQueryName(PropertyIds.NAME);
		props.addProperty(p);

		String content = "aegif Mind Share Leader Generating New Paradigms by aegif corporation.";
		byte[] buf = null;
		try {
			buf = content.getBytes("UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		ByteArrayInputStream input = new ByteArrayInputStream(buf);
		ContentStream contentStream = new ContentStreamImpl("pippo.txt", new BigInteger("" + buf.length),
				"text/plain; fileNameCharset=UTF-8", input);

		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);
		log.info(folder.getName());

		Session session = SessionManager.get().newSession("admin", "admin", null);

		LDRepository ldrep = new LDRepository(folder, session.getSid());
		String id = ldrep.createDocument(null, props, "fld.4", contentStream);
		assertNotNull(id);

	}

	@Test
	public void testCancelCheckOut() throws PersistenceException {

		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);

		Document document = new Document();
		document.setFileName("newDocument");
		document.setFolder(folder);
		document.setStatus(2);
		ddao.store(document);

		testSubject.cancelCheckOut("doc." + document.getId());

		document = ddao.findById(document.getId());
		assertEquals(0, document.getStatus());
	}

	@Test
	public void testGetAllVersions() throws PersistenceException {
		VersionDAO vd = (VersionDAO) context.getBean("VersionDAO");
		List<Version> versions = vd.findByDocId(1L);
		assertEquals(2, versions.size());

		List<ObjectData> list = testSubject.getAllVersions("doc.1");
		assertEquals(2, list.size());
	}

	@Test
	public void testGetContentChanges() throws PersistenceException, ParseException {
		Document document = ddao.findById(1L);
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		DocumentHistoryDAO dao = (DocumentHistoryDAO) context.getBean("DocumentHistoryDAO");
		DocumentHistory hist = new DocumentHistory();
		hist.setEvent(DocumentEvent.STORED.toString());
		hist.setDate(df.parse("2019-15-12"));
		hist.setDocument(document);
		dao.store(hist);

		hist = new DocumentHistory();
		hist.setEvent(DocumentEvent.CHANGED.toString());
		hist.setDate(df.parse("2020-15-12"));
		hist.setDocument(document);
		dao.store(hist);

		hist.setEvent(DocumentEvent.MOVED.toString());
		hist.setDate(df.parse("2018-15-12"));
		hist.setDocument(document);
		dao.store(hist);

		hist.setEvent(DocumentEvent.DELETED.toString());
		hist.setDate(df.parse("2021-15-12"));
		hist.setDocument(document);
		dao.store(hist);

		Folder folder = fdao.findById(4L);
		FolderHistoryDAO folderHistoryDao = (FolderHistoryDAO) context.getBean("FolderHistoryDAO");
		FolderHistory folderHistory = new FolderHistory();
		folderHistory.setEvent(FolderEvent.CREATED.toString());
		folderHistory.setDate(df.parse("2018-15-12"));
		folderHistory.setDocument(document);
		folderHistory.setFolder(folder);
		folderHistoryDao.store(folderHistory);

		folderHistory = new FolderHistory();
		folderHistory.setEvent(FolderEvent.RENAMED.toString());
		folderHistory.setDate(df.parse("2019-15-12"));
		folderHistory.setDocument(document);
		folderHistory.setFolder(folder);
		folderHistoryDao.store(folderHistory);

		folderHistory = new FolderHistory();
		folderHistory.setEvent(FolderEvent.CHANGED.toString());
		folderHistory.setDate(df.parse("2020-15-12"));
		folderHistory.setDocument(document);
		folderHistory.setFolder(folder);
		folderHistoryDao.store(folderHistory);

		folderHistory = new FolderHistory();
		folderHistory.setEvent(FolderEvent.DELETED.toString());
		folderHistory.setDate(df.parse("2021-15-12"));
		folderHistory.setDocument(document);
		folderHistory.setFolder(folder);
		folderHistoryDao.store(folderHistory);

		Holder<String> holder = new Holder<String>("123");

		ObjectList ol = testSubject.getContentChanges(holder, 10);
		List<ObjectData> results = ol.getObjects();
		for (Iterator<ObjectData> iterator = results.iterator(); iterator.hasNext();) {
			ObjectData objectData = (ObjectData) iterator.next();

			PropertyData<?> oid = objectData.getProperties().getProperties().get("cmis:objectId");

			String cmisDocID = (String) oid.getFirstValue();
			assertTrue(cmisDocID.equals("doc.1") || cmisDocID.equals("fld.5"));
		}

	}

	@Test
	public void testGetFolderParent() {
		ObjectData od = testSubject.getFolderParent(new MockCallContext(null, session.getSid()), "fld.4");
		assertNotNull(od);
		assertEquals("fld.5", od.getId());
	}

	@Test
	public void testGetObjectParents() {
		List<ObjectParentData> docList = testSubject.getObjectParents(new MockCallContext(null, session.getSid()),
				"doc.1", null, true, true, null);
		List<ObjectParentData> fldList = testSubject.getObjectParents(new MockCallContext(null, session.getSid()),
				"fld.5", "x", true, true, null);
		assertTrue(docList.stream().filter(dL -> "fld.5".equals(dL.getObject().getId())).count() >= 1);
		assertTrue(fldList.stream().filter(fL -> "fld.5".equals(fL.getObject().getId())).count() >= 1);
	}

	@Test
	public void testGetChildren() throws PersistenceException {
		Folder folder = fdao.findById(4);
		assertEquals(5, folder.getParentId());
		ObjectInFolderList oifl = testSubject.getChildren(new MockCallContext(null, session.getSid()), "fld.5", null,
				false, true, 10, 0, null);
		assertTrue(oifl.getObjects().stream().filter(oi -> "fld.4".equals(oi.getObject().getId())).count() >= 1);

		oifl = testSubject.getChildren(new MockCallContext(null, session.getSid()), "fld.5", null, false, false, 1, 0,
				null);
		assertTrue(oifl.getObjects().stream().filter(oi -> "fld.4".equals(oi.getObject().getId())).count() >= 1);

	}

	@Test
	public void testGetAllowableActions() {
		AllowableActions aA = testSubject.getAllowableActions(new MockCallContext(null, session.getSid()), "doc.1");
		assertNotNull(aA);
	}

	@Test
	public void testGetAcl() {
		Acl acl = testSubject.getAcl(new MockCallContext(null, session.getSid()), "doc.1");
		assertNotNull(acl);
	}

	@Test
	public void testGetObjectByPath() throws IOException, PersistenceException {
		Document doc = ddao.findById(5L);
		Folder folder = doc.getFolder();
		String path = fdao.computePathExtended(folder) + "/" + doc.getFileName();

		ObjectData od = testSubject.getObjectByPath(null, path, null, false, null, null, false, false, null);
		assertNotNull(od);
		assertEquals("doc.5", od.getId());

		try {
			testSubject.getObjectByPath(null, "/Default/unexisting", null, false, null, null, false, false, null);
			fail("No expception in presence of unexisting pat?");
		} catch (CmisObjectNotFoundException conf) {
			// It's expected an exception here
		}
	}

	@Test
	public void testGetContentStream() throws PersistenceException, IOException {
		Document doc2 = ddao.findById(2L);
		String doc2Name = doc2.getFileName();

		String storePath = Context.get().getProperties().getProperty("store.1.dir");
		File store = new File(storePath);
		File folder1 = new File(store, "1");
		File docFolder = new File(folder1, "doc");
		File file1_0 = new File(docFolder, "1.0");
		File fileVer = new File(docFolder, "fileVer01");
		folder1.mkdir();
		docFolder.mkdirs();
		file1_0.createNewFile();
		fileVer.createNewFile();

		ContentStream contentStreamDocument = testSubject.getContentStream(null, "doc.2", null, null);
		ContentStream contentStreamFolder = testSubject.getContentStream(null, "ver.1", null, null);
		assertEquals(doc2Name, contentStreamDocument.getFileName());
		assertNotNull(contentStreamFolder);
	}

	@Test
	public void testGetObject() {
		ObjectData od1 = testSubject.getObject(null, "fld.4", null, null, true, true, null);
		ObjectData od2 = testSubject.getObject(null, null, "fld.4", null, true, true, null);

		Map<String, PropertyData<?>> properties1 = od1.getProperties().getProperties();
		for (Map.Entry<String, PropertyData<?>> entry : properties1.entrySet()) {
			String key = entry.getKey();
			PropertyData<?> propertyData = entry.getValue();
			if (key.equals("cmis:objectId")) {
				assertTrue(propertyData.getValues().contains("fld.4"));
			}
		}

		Map<String, PropertyData<?>> properties2 = od2.getProperties().getProperties();
		for (Map.Entry<String, PropertyData<?>> entry : properties2.entrySet()) {
			String key = entry.getKey();
			PropertyData<?> propertyData = entry.getValue();
			if (key.equals("cmis:objectId")) {
				assertTrue(propertyData.getValues().contains("fld.4"));
			}
		}

	}

	@Test
	public void testCheckOut() throws PersistenceException {
		int status = ddao.findById(1L).getStatus();
		assertEquals(0, status);

		Holder<String> stringHolder = new Holder<String>("doc.1");
		Holder<Boolean> booleanHolder = new Holder<Boolean>(true);
		testSubject.checkOut(stringHolder, booleanHolder);

		status = ddao.findById(1L).getStatus();
		assertEquals(1, status);
	}

	@Test
	public void testCheckIn() throws PersistenceException, UnsupportedEncodingException {
		int status = ddao.findById(6L).getStatus();
		assertEquals(0, status);

		Holder<String> stringHolder = new Holder<String>("doc.6");
		Holder<Boolean> booleanHolder = new Holder<Boolean>(true);
		testSubject.checkOut(stringHolder, booleanHolder);

		status = ddao.findById(6L).getStatus();
		assertEquals(1, status);

		ObjectInfoImpl cmisObject = (ObjectInfoImpl) testSubject.getObjectInfo("doc.6", null);
		assertNotNull(cmisObject);
		assertNotNull(cmisObject.getObject());
		PropertiesImpl props = (PropertiesImpl) cmisObject.getObject().getProperties();
		props.removeProperty(PropertyIds.OBJECT_ID);
		props.removeProperty(PropertyIds.LAST_MODIFICATION_DATE);
		props.removeProperty(PropertyIds.CHANGE_TOKEN);
		props.removeProperty(PropertyIds.CREATED_BY);
		props.removeProperty(PropertyIds.LAST_MODIFIED_BY);
		props.removeProperty(PropertyIds.CREATION_DATE);
		props.removeProperty(PropertyIds.BASE_TYPE_ID);
		props.removeProperty(PropertyIds.IS_LATEST_VERSION);
		props.removeProperty(PropertyIds.IS_MAJOR_VERSION);
		props.removeProperty(PropertyIds.VERSION_LABEL);
		props.removeProperty(PropertyIds.VERSION_SERIES_ID);
		props.removeProperty(PropertyIds.IS_VERSION_SERIES_CHECKED_OUT);
		props.removeProperty(PropertyIds.VERSION_SERIES_CHECKED_OUT_BY);
		props.removeProperty(PropertyIds.VERSION_SERIES_CHECKED_OUT_ID);
		props.removeProperty(PropertyIds.CHECKIN_COMMENT);
		props.removeProperty(PropertyIds.CONTENT_STREAM_LENGTH);
		props.removeProperty(PropertyIds.CONTENT_STREAM_MIME_TYPE);
		props.removeProperty(PropertyIds.CONTENT_STREAM_FILE_NAME);
		props.removeProperty(PropertyIds.CONTENT_STREAM_ID);
		props.removeProperty(PropertyIds.IS_IMMUTABLE);
		props.removeProperty(TypeManager.PROP_RATING);
		props.removeProperty(TypeManager.PROP_FILEVERSION);
		props.removeProperty(TypeManager.PROP_VERSION);

		String content = "aegif Mind Share Leader Generating New Paradigms by aegif corporation.";
		byte[] buf = content.getBytes("UTF-8");
		ByteArrayInputStream input = new ByteArrayInputStream(buf);
		ContentStream contentStream = new ContentStreamImpl("pippo.txt", new BigInteger("" + buf.length),
				"text/plain; fileNameCharset=UTF-8", input);

		stringHolder = new Holder<String>("doc.6");

		testSubject.checkIn(stringHolder, true, contentStream, props, content);

		status = ddao.findById(6L).getStatus();
		assertEquals(0, status);
	}

	@Test
	public void testCreateFolder() throws PersistenceException {
		PropertiesImpl props = new PropertiesImpl();

		PropertyIdImpl p = new PropertyIdImpl(PropertyIds.OBJECT_TYPE_ID, "cmis:document");
		p.setQueryName(PropertyIds.OBJECT_TYPE_ID);
		props.addProperty(p);

		p = new PropertyIdImpl(PropertyIds.NAME, "pippo.txt");
		p.setQueryName(PropertyIds.NAME);
		props.addProperty(p);

		Folder folder = fdao.findDefaultWorkspace(Tenant.DEFAULT_ID);
		Session session = SessionManager.get().newSession("admin", "admin", null);

		List<Folder> list = fdao.findAll();
		assertEquals(7, list.size());

		LDRepository ldrep = new LDRepository(folder, session.getSid());
		String id = ldrep.createFolder(null, props, "fld.4");
		assertNotNull(id);

		list = fdao.findAll();
		assertEquals(8, list.size());
		assertEquals("pippo.txt", list.get(4).getName());

		ldrep = new LDRepository(folder, null);
		id = ldrep.createFolder(new MockCallContext(null, session.getSid()), props, "fld.4");
		assertNotNull(id);

	}

	@Test
	public void testMoveObject() throws PersistenceException {
		Holder<String> stringHolder = new Holder<String>("doc.1");

		Document document = ddao.findById(1L);
		assertEquals(5, document.getFolder().getId());

		Folder folder = fdao.findById(5);
		fdao.initialize(folder);
		testSubject.moveObject(null, stringHolder, "fld.4", null);

		document = ddao.findById(1L);
		assertEquals(4, document.getFolder().getId());

		// now try to move a folder
		Folder folder6 = fdao.findById(6L);
		fdao.initialize(folder6);
		assertEquals(5, folder6.getParentId());

		stringHolder = new Holder<String>("fld.6");
		testSubject.moveObject(null, stringHolder, "fld.4", null);

		folder6 = fdao.findById(6L);
		fdao.initialize(folder6);
		assertEquals(4, folder6.getParentId());

	}

	@Test
	public void testAppendContent() throws PersistenceException, IOException {
		// Upload a first chunk
		File content = new File("target/content1.txt");
		FileUtil.writeFile("content1", content.getPath());
		try (FileInputStream fis = new FileInputStream(content)) {
			ContentStream contentStream = new ContentStreamImpl("file.txt", new BigInteger("" + content.length()),
					"text/plain; fileNameCharset=UTF-8", fis);
			testSubject.appendContent(null, "doc.5", contentStream, false);
		} finally {
			FileUtil.strongDelete(content);
		}

		// Upload a second chunk
		content = new File("target/content2.txt");
		FileUtil.writeFile("content2", content.getPath());
		try (FileInputStream fis = new FileInputStream(content)) {
			ContentStream contentStream = new ContentStreamImpl("file.txt", new BigInteger("" + content.length()),
					"text/plain; fileNameCharset=UTF-8", fis);
			testSubject.appendContent(null, "doc.5", contentStream, true);
		} finally {
			FileUtil.strongDelete(content);
		}

		Storer storer = (Storer) context.getBean("Storer");
		String mergedContent = new String(storer.getBytes(5L, "1.0"));
		assertTrue(mergedContent.contains("content1"));
		assertTrue(mergedContent.contains("content2"));

	}

	@Test
	public void testDeleteObject() throws PersistenceException {
		Folder folder = fdao.findById(6L);
		assertNotNull(folder);

		testSubject.deleteObject(null, "fld.6");

		folder = fdao.findById(6L);
		assertNull(folder);

		Document document = ddao.findById(1L);
		assertNotNull(document);

		testSubject.deleteObject(null, "doc.1");

		document = ddao.findById(1L);
		assertNull(document);
	}

	@Test
	public void testDeleteObjectOrCancelCheckOut() throws PersistenceException {
		int status = ddao.findById(1L).getStatus();
		assertEquals(0, status);

		Holder<String> stringHolder = new Holder<String>("doc.1");
		Holder<Boolean> booleanHolder = new Holder<Boolean>(true);
		testSubject.checkOut(stringHolder, booleanHolder);

		status = ddao.findById(1L).getStatus();
		assertEquals(1, status);

		Document doc = ddao.findById(1L);
		testSubject.deleteObjectOrCancelCheckOut(null, "doc.1");

		status = ddao.findById(1L).getStatus();
		assertEquals(0, status);

		doc = ddao.findById(1L);
		assertEquals(0, doc.getDeleted());

		testSubject.deleteObjectOrCancelCheckOut(null, "doc.1");
		doc = ddao.findById(1L);
		assertNull(doc);

		Folder folder = fdao.findById(6L);
		assertEquals(0, folder.getDeleted());

		testSubject.deleteObjectOrCancelCheckOut(null, "fld.6");
		folder = fdao.findById(6L);
		assertNull(folder);
	}

	@Test
	public void testCreate() throws FileNotFoundException, IOException {
		// test document
		PropertiesImpl props = new PropertiesImpl();

		PropertyIdImpl p = new PropertyIdImpl(PropertyIds.OBJECT_TYPE_ID, "cmis:document");
		p.setQueryName(PropertyIds.OBJECT_TYPE_ID);
		props.addProperty(p);

		p = new PropertyIdImpl(PropertyIds.NAME, "pippo.txt");
		p.setQueryName(PropertyIds.NAME);
		props.addProperty(p);

		String content = "aegif Mind Share Leader Generating New Paradigms by aegif corporation.";
		byte[] buf = null;
		try {
			buf = content.getBytes("UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		ByteArrayInputStream input = new ByteArrayInputStream(buf);
		ContentStream contentStream = new ContentStreamImpl("pippo.txt", new BigInteger("" + buf.length),
				"text/plain; fileNameCharset=UTF-8", input);

		ObjectData oj = testSubject.create(null, props, "fld.4", contentStream, null);
		assertNotNull(oj);

		// test folder
		PropertiesImpl properties = new PropertiesImpl();

		PropertyIdImpl folder = new PropertyIdImpl(PropertyIds.OBJECT_TYPE_ID, "cmis:folder");
		folder.setQueryName(PropertyIds.OBJECT_TYPE_ID);
		properties.addProperty(folder);

		folder = new PropertyIdImpl(PropertyIds.NAME, "folder6");
		folder.setQueryName(PropertyIds.NAME);
		properties.addProperty(folder);

		oj = testSubject.create(null, properties, "fld.4", contentStream, null);
		assertNotNull(oj);
	}

	@Test
	public void testDeleteTree() throws PersistenceException {
		Folder folder = fdao.findById(6L);
		assertNotNull(folder);
		FailedToDeleteData ftdd = testSubject.deleteTree(null, "fld.6", false);
		assertNotNull(ftdd);
		folder = fdao.findById(6L);
		assertNull(folder);

		Document document = ddao.findById(1L);
		assertNotNull(document);
		ftdd = testSubject.deleteTree(null, "doc.1", false);
		assertNotNull(ftdd);
		document = ddao.findById(1L);
		assertNull(document);

	}

}
