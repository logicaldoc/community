package com.logicaldoc.core.searchengine;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.lucene.index.CheckIndex;
import org.apache.lucene.index.CheckIndex.Status;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.LockObtainFailedException;
import org.apache.lucene.store.NIOFSDirectory;
import org.apache.lucene.util.Version;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrQuery.SortClause;
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CursorMarkParams;
import org.apache.solr.core.CoreContainer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Joiner;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.document.dao.DocumentNoteDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.searchengine.analyzer.FilteredAnalyzer;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * Standard implementation that implements a local search engine
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class StandardSearchEngine implements SearchEngine {

	public static Version VERSION = Version.LUCENE_8_11_2;

	protected static Logger log = LoggerFactory.getLogger(StandardSearchEngine.class);

	private ContextProperties config;

	protected DocumentDAO documentDao;

	protected DocumentNoteDAO noteDao;

	protected EmbeddedSolrServer server;

	protected StandardSearchEngine() {
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	public void setDocumentDao(DocumentDAO documentDao) {
		this.documentDao = documentDao;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.logicaldoc.core.searchengine.SearchEngine#addHit(com.logicaldoc.core
	 * .document.Document, java.lang.String)
	 */
	@Override
	public synchronized void addHit(Document document, String content) throws Exception {
		documentDao.initialize(document);
		Document doc = document;

		if (document.getDocRef() != null) {
			// This is an alias
			Document referencedDoc = documentDao.findById(document.getDocRef());
			documentDao.initialize(referencedDoc);
			doc = new Document(referencedDoc);
			doc.setId(document.getId());
			doc.setTenantId(document.getTenantId());
			doc.setDocRef(document.getDocRef());
			doc.setDocRefType(document.getDocRefType());
			doc.setFolder(document.getFolder());
		}

		SolrInputDocument hit = new SolrInputDocument();

		hit.addField(HitField.ID.getName(), Long.toString(doc.getId()));
		hit.addField(HitField.TENANT_ID.getName(), Long.toString(doc.getTenantId()));
		hit.addField(HitField.LANGUAGE.getName(), doc.getLanguage());
		hit.addField(HitField.FILENAME.getName(), doc.getFileName());
		hit.addField(HitField.TITLE.getName(), doc.getTitle());
		hit.addField(HitField.SIZE.getName(), doc.getFileSize());
		hit.addField(HitField.DATE.getName(), doc.getDate());
		hit.addField(HitField.CREATION.getName(), doc.getCreation());
		hit.addField(HitField.CUSTOM_ID.getName(), doc.getCustomId());
		hit.addField(HitField.COMMENT.getName(), doc.getComment());
		hit.addField(HitField.TAGS.getName(), doc.getTagsString());
		hit.addField(HitField.DOC_REF.getName(), doc.getDocRef());

		int maxText = -1;
		if (StringUtils.isNotEmpty(config.getProperty("index.maxtext"))) {
			try {
				maxText = config.getInt("index.maxtext");
			} catch (Exception e) {
			}
		}

		if (content != null) {
			String utf8Content = StringUtil.removeNonUtf8Chars(content);
			if (maxText > 0 && utf8Content.length() > maxText)
				hit.addField(HitField.CONTENT.getName(), StringUtils.substring(utf8Content, 0, maxText));
			else
				hit.addField(HitField.CONTENT.getName(), utf8Content);
		}

		if (doc.getFolder() != null) {
			hit.addField(HitField.FOLDER_ID.getName(), doc.getFolder().getId());
			hit.addField(HitField.FOLDER_NAME.getName(), doc.getFolder().getName());
		}

		if (doc.getTemplateId() != null) {
			hit.addField(HitField.TEMPLATE_ID.getName(), doc.getTemplateId());

			for (String attribute : doc.getAttributeNames()) {
				Attribute ext = doc.getAttribute(attribute);
				if (StringUtils.isNotEmpty(ext.getParent()))
					continue;

				// Skip all non-string attributes
				if ((ext.getType() == Attribute.TYPE_STRING || ext.getType() == Attribute.TYPE_USER)
						&& (StringUtils.isNotEmpty(ext.getStringValue())
								|| StringUtils.isNotEmpty(ext.getStringValues()))) {

					// Prefix all extended attributes with 'ext_' in order to
					// avoid collisions with standard fields
					hit.addField("ext_" + attribute,
							StringUtils.isNotEmpty(ext.getStringValues()) ? ext.getStringValues()
									: ext.getStringValue());
				}
			}
		}

		// Retrieve the notes
		StringBuffer sb = new StringBuffer();
		List<DocumentNote> notes = noteDao.findByDocId(doc.getId(), doc.getFileVersion());
		for (DocumentNote note : notes) {
			if (sb.length() > 0)
				sb.append("\n\n");
			sb.append(note.getMessage());
		}
		if (sb.length() > 0)
			hit.addField(HitField.NOTES.getName(), sb.toString());

		try {
			FilteredAnalyzer.lang.set(doc.getLanguage());
			server.add(hit);
			server.commit();
		} finally {
			FilteredAnalyzer.lang.remove();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.logicaldoc.core.searchengine.SearchEngine#addHit(com.logicaldoc.core
	 * .document.Document, java.io.InputStream)
	 */
	@Override
	public synchronized void addHit(Document document, InputStream content) throws Exception {
		Document doc = document;
		if (doc.getDocRef() != null)
			doc = documentDao.findById(doc.getDocRef());

		Locale locale = doc.getLocale();
		if (locale == null)
			locale = Locale.ENGLISH;

		String contentString = null;

		if (doc.getIndexed() != AbstractDocument.INDEX_TO_INDEX_METADATA)
			ParserFactory.parse(content, doc.getFileName(), null, locale, doc.getTenantId(), doc, null);

		addHit(doc, contentString);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.logicaldoc.core.searchengine.SearchEngine#optimize()
	 */
	@Override
	public synchronized void optimize() {
		log.warn("Started optimization of the index");
		try {
			server.optimize(true, true);
		} catch (Exception e) {
			log.error("Error during optimization: " + e.getMessage(), e);
		}
		log.warn("Finished optimization of the index");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.logicaldoc.core.searchengine.SearchEngine#check()
	 */
	@Override
	public String check() {
		log.warn("Checking index");

		
		String statMsg = "";
		
		try(CheckIndex ci = new CheckIndex(getIndexDataDirectory()); ByteArrayOutputStream baos =new ByteArrayOutputStream();) {
			PrintStream ps = new PrintStream(baos);

			// Retrieve the status collecting all informations in a string
			Status status = null;
			ci.setInfoStream(ps);
			try {
				status = ci.checkIndex();
			} catch (Exception e) {
				ps.println("ERROR: caught exception, giving up.\n\n");
				log.error(e.getMessage());
			}

			// Elaborate the status showing needed informations
			if (status != null) {
				if (status.clean) {
					statMsg = "OK\n";
				} else if (status.toolOutOfDate) {
					statMsg = "ERROR: Can't check - tool out-of-date\n";
				} else {
					statMsg = "BAD: ";
					if (status.missingSegments) {
						statMsg += "missingSegments ";
					}
					if (status.numBadSegments > 0) {
						statMsg += "numBadSegments=" + status.numBadSegments + " ";
					}
					if (status.totLoseDocCount > 0) {
						statMsg += "lostDocCount=" + status.totLoseDocCount + " ";
					}
				}

				String content = "";
				try {
					content = baos.toString("UTF-8");
				} catch (UnsupportedEncodingException e) {
				}

				statMsg += "\n" + content;
			}
		} catch (Throwable t) {
			log.error(t.getMessage());
		}
		
		log.warn("Finished checking index");
		return statMsg;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.logicaldoc.core.searchengine.SearchEngine#deleteHit(long)
	 */
	@Override
	public synchronized void deleteHit(long id) {
		try {
			server.deleteById(Long.toString(id));
			server.commit();
		} catch (Throwable e) {
			log.debug("Unable to delete hit {}", id, e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.logicaldoc.core.searchengine.SearchEngine#deleteHits(java.util.
	 * Collection )
	 */
	@Override
	public synchronized void deleteHits(Collection<Long> ids) {
		try {
			server.deleteById(ids.stream().map(i -> Long.toString(i)).collect(Collectors.toList()));
			server.commit();
		} catch (Throwable e) {
			log.debug("Unable to delete {} hits", ids.size(), e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.logicaldoc.core.searchengine.SearchEngine#getHit(long)
	 */
	@Override
	public Hit getHit(long id) {
		SolrQuery query = new SolrQuery();
		query.setQuery("id:" + id);
		query.setFields("*");
		try {
			QueryResponse rsp = server.query(query);
			SolrDocumentList docs = rsp.getResults();
			if (docs.size() < 1)
				return null;

			SolrDocument doc = docs.get(0);
			Hit hit = Hits.toHit(doc);
			hit.setContent((String) doc.getFieldValue(HitField.CONTENT.getName()));
			return hit;
		} catch (Throwable e) {
			log.error(e.getMessage());
		}
		return null;
	}

	@Override
	public Hits query(String expression, int page, int size) {
		SolrQuery query = (new SolrQuery(expression)).setRows(size).setStart((page - 1) * size)
				.setSort(SortClause.asc("id"));
		Hits hits = null;
		try {
			QueryResponse rsp = server.query(query);
			hits = new Hits(rsp);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
		return hits;
	}

	@Override
	public Hits search(String expression, String[] filters, String expressionLanguage, Integer rows) {
		try {
			// This configures the analyzer to use to to parse the expression of
			// the content field
			FilteredAnalyzer.lang.set(expressionLanguage);
			Hits hits = null;
			SolrQuery query = prepareSearchQuery(expression, filters, expressionLanguage, rows);

			try {
				log.info("Execute search: {}", expression);
				QueryResponse rsp = server.query(query);
				hits = new Hits(rsp);
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}
			return hits;
		} finally {
			FilteredAnalyzer.lang.remove();
		}
	}

	/**
	 * Prepares the query for a search.
	 */
	protected SolrQuery prepareSearchQuery(String expression, String[] filters, String expressionLanguage,
			Integer rows) {
		SolrQuery query = new SolrQuery().setQuery(expression);
		if (rows != null)
			query = query.setRows(rows);
		if (filters != null)
			query = query.addFilterQuery(Joiner.on(" +").join(filters));
		query = query.setSort(SortClause.desc("score"));
		query.set("exprLang", expressionLanguage);
		return query;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.logicaldoc.core.searchengine.SearchEngine#close()
	 */
	@Override
	public synchronized void close() {
		log.warn("Closing the indexer");
		try {
			server.commit();
			unlock();
			server.getCoreContainer().shutdown();
			server.close();
			FileUtil.strongDelete(new File(getIndexDataFolder(), IndexWriter.WRITE_LOCK_NAME));
		} catch (Throwable e) {
			log.warn(e.getMessage(), e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.logicaldoc.core.searchengine.SearchEngine#unlock()
	 */
	@Override
	public synchronized void unlock() {
		try {
			Directory directory = getIndexDataDirectory();
			if (isLocked())
				directory.obtainLock(IndexWriter.WRITE_LOCK_NAME).close();
		} catch (Throwable e) {
			log.warn("unlock {}", e.getMessage());
			try {
				FileUtil.strongDelete(new File(getIndexDataFolder(), "write.lock"));
			} catch (Throwable e1) {
				log.warn("unlock {}", e1.getMessage());
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.logicaldoc.core.searchengine.SearchEngine#isLocked()
	 */
	@Override
	public boolean isLocked() {
		boolean result = false;

		try {
			Directory directory = getIndexDataDirectory();
			try {
				directory.obtainLock(IndexWriter.WRITE_LOCK_NAME).close();
				result = false;
			} catch (LockObtainFailedException failed) {
				result = true;
			}
		} catch (Throwable e) {
			log.warn("isLocked {}", e.getMessage(), e);
		}

		return result;
	}

	/**
	 * @see com.logicaldoc.core.searchengine.SearchEngine#getCount()
	 */
	@Override
	public long getCount() {
		SolrQuery query = new SolrQuery();
		query.setQuery("*:*");
		try {
			QueryResponse rsp = server.query(query);
			SolrDocumentList docs = rsp.getResults();
			return docs.getNumFound();
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
		return 0;
	}

	@Override
	public long purge() {
		log.info("Purging the index");

		/*
		 * Remove from the index all the entries related to deleted files
		 */
		List<Long> deletedDocs = documentDao.findDeletedDocIds();
		deleteHits(deletedDocs);
		log.info("{} entries marked as deleted because refer to deleted documents", deletedDocs.size());

		/*
		 * Iterate over all the entries in pages of 1000 elements checking if
		 * the entry refers to an existing document. We use the cursor mark
		 * method.
		 */
		SolrQuery query = (new SolrQuery("*:*")).setRows(1000).setSort(SortClause.asc("id"));
		String cursorMark = CursorMarkParams.CURSOR_MARK_START;
		boolean done = false;

		long deletedCount = 0;
		try {
			while (!done) {
				query.set(CursorMarkParams.CURSOR_MARK_PARAM, cursorMark);
				QueryResponse rsp = server.query(query);
				String nextCursorMark = rsp.getNextCursorMark();

				List<Long> hitsToDelete = new ArrayList<Long>();
				SolrDocumentList docs = rsp.getResults();
				for (SolrDocument doc : docs) {
					long count = documentDao
							.queryForLong("select count(ld_id) from ld_document where ld_deleted=0 and ld_id="
									+ doc.get(HitField.ID.getName()));
					if (count < 1)
						hitsToDelete.add(Long.parseLong(doc.get(HitField.ID.getName()).toString()));
				}

				deleteHits(deletedDocs);
				deletedCount += deletedDocs.size();

				if (cursorMark.equals(nextCursorMark))
					done = true;
				cursorMark = nextCursorMark;
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}

		log.info("{} entries were removed from the index because refer to unexisting documents", deletedCount);
		return deletedCount;

	}

	/**
	 * @see com.logicaldoc.core.searchengine.SearchEngine#dropIndex()
	 */
	@Override
	public void dropIndex() {
		try {
			server.deleteByQuery("*:*");
			server.optimize();
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}

	Directory getIndexDataDirectory() throws IOException {
		return new NIOFSDirectory(getIndexDataFolder().toPath());
	}

	File getIndexDataFolder() throws IOException {
		File indexdir = new File(config.getPropertyWithSubstitutions("index.dir"));
		indexdir = new File(indexdir, "logicaldoc");
		indexdir = new File(indexdir, "data");
		return new File(indexdir, "index");
	}

	/**
	 * @see com.logicaldoc.core.searchengine.SearchEngine#init()
	 */
	@Override
	public void init() {
		try {
			File indexHome = new File(config.getPropertyWithSubstitutions("index.dir"));
			File solr_xml = new File(indexHome, "solr.xml");

			if (!indexHome.exists()) {
				indexHome.mkdirs();
				indexHome.mkdir();
			}
			if (!solr_xml.exists()) {
				FileUtil.copyResource("/index/solr.xml", solr_xml);
			}

			File ldoc = new File(config.getPropertyWithSubstitutions("index.dir"));
			ldoc = new File(ldoc, "logicaldoc");
			if (!ldoc.exists()) {
				ldoc.mkdirs();
				ldoc.mkdir();
			}
			File core_prop = new File(ldoc, "core.properties");
			if (!core_prop.exists())
				FileUtil.copyResource("/index/logicaldoc/core.properties", core_prop);

			File conf = new File(ldoc, "conf");
			if (!conf.exists()) {
				conf.mkdirs();
				conf.mkdir();
			}
			File solrconfig_xml = new File(conf, "solrconfig.xml");
			if (!solrconfig_xml.exists()) {
				FileUtil.copyResource("/index/logicaldoc/conf/solrconfig.xml", solrconfig_xml);
			}
			File schema_xml = new File(conf, "schema.xml");
			if (!schema_xml.exists()) {
				FileUtil.copyResource("/index/logicaldoc/conf/schema.xml", schema_xml);
			}
			File synonyms_txt = new File(conf, "synonyms.txt");
			if (!synonyms_txt.exists()) {
				FileUtil.copyResource("/index/logicaldoc/conf/synonyms.txt", synonyms_txt);
			}
			File protwords_txt = new File(conf, "protwords.txt");
			if (!protwords_txt.exists()) {
				FileUtil.copyResource("/index/logicaldoc/conf/protwords.txt", protwords_txt);
			}
			File alphatypes_txt = new File(conf, "alphatypes.txt");
			if (!alphatypes_txt.exists()) {
				FileUtil.copyResource("/index/logicaldoc/conf/alphatypes.txt", alphatypes_txt);
			}

			// Delete the lock file if it exists
			FileUtil.strongDelete(new File(indexHome, "logicaldoc/data/index/" + IndexWriter.WRITE_LOCK_NAME));

			CoreContainer container = new CoreContainer(indexHome.toPath(), null);
			server = new EmbeddedSolrServer(container, "logicaldoc");
			container.load();

			unlock();
		} catch (Exception e) {
			log.error("Unable to initialize the Full-text search engine", e);
		}
	}

	@Override
	public Object getServer() {
		return server;
	}

	public void setNoteDao(DocumentNoteDAO noteDao) {
		this.noteDao = noteDao;
	}
}