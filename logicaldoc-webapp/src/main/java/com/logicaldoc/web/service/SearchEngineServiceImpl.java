package com.logicaldoc.web.service;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.Hits;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.searchengine.SearchException;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;
import com.logicaldoc.gui.frontend.client.services.SearchEngineService;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * Implementation of the SearchEngineService
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SearchEngineServiceImpl extends AbstractRemoteService implements SearchEngineService {

	private static final String INDEX_TOKENFILTER = "index.tokenfilter.";

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(SearchEngineServiceImpl.class);

	@Override
	public GUISearchEngine getInfo() throws ServerException {
		Session session = validateSession();

		try {
			GUISearchEngine searchEngine = new GUISearchEngine();

			SearchEngine indexer = Context.get(SearchEngine.class);
			searchEngine.setLocked(indexer.isLocked());

			ContextProperties conf = Context.get().getProperties();
			searchEngine.setExcludePatterns(conf.getProperty(session.getTenantName() + ".index.excludes"));
			searchEngine.setIncludePatterns(conf.getProperty(session.getTenantName() + ".index.includes"));
			searchEngine
					.setExcludePatternsMetadata(conf.getProperty(session.getTenantName() + ".index.excludes.metadata"));
			searchEngine
					.setIncludePatternsMetadata(conf.getProperty(session.getTenantName() + ".index.includes.metadata"));
			searchEngine.setSkipOnError(conf.getBoolean(session.getTenantName() + ".index.skiponerror", false));
			searchEngine.setParsingTimeout(conf.getInt(session.getTenantName() + ".parser.timeout", 0));
			searchEngine
					.setParsingTimeoutRetain(conf.getBoolean(session.getTenantName() + ".parser.timeout.retain", true));
			searchEngine.setMaxTextFileSize(conf.getInt(session.getTenantName() + ".parser.txt.maxsize", 0));
			searchEngine.setDir(conf.getProperty("index.dir"));
			searchEngine.setSorting(conf.getProperty("index.sorting"));
			searchEngine.setCustomSorting(conf.getProperty("index.sorting.custom"));
			searchEngine.setThreads(conf.getInt("threadpool.IndexerTask.max", 2));
			searchEngine.setBatch(conf.getInt("index.batch", 0));
			searchEngine.setMaxText(conf.getInt("index.maxtext", 0));

			// Populate the list of supported languages
			searchEngine.setLanguages("");
			LanguageManager lm = LanguageManager.getInstance();
			List<String> langs = lm.getLanguagesAsString(session.getTenantName());
			for (String lang : langs) {
				searchEngine.setLanguages(searchEngine.getLanguages() + "," + lang);
			}
			if (searchEngine.getLanguages().startsWith(","))
				searchEngine.setLanguages(searchEngine.getLanguages().substring(1));

			return searchEngine;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public void rescheduleAll(final boolean dropIndex) throws ServerException {
		final Session session = validateSession();

		if (dropIndex)
			try {
				Context.get(SearchEngine.class).dropIndex();
			} catch (Exception e) {
				throw new ServerException(e.getMessage(), e);
			}

		Runnable task = () -> {
			try {
				Context.get(DocumentDAO.class).jdbcUpdate("update ld_document set ld_indexed=0 where ld_indexed=1 "
						+ (!dropIndex ? " and ld_tenantid=" + session.getTenantId() : ""));
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			}
		};

		new Thread(task).start();
	}

	@Override
	public void unlock() throws ServerException {
		Session session = validateSession();

		try {
			SearchEngine indexer = Context.get(SearchEngine.class);
			indexer.unlock();
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public String check() throws ServerException {
		Session session = validateSession();

		try {
			SearchEngine indexer = Context.get(SearchEngine.class);
			return indexer.check();
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public void save(GUISearchEngine searchEngine) throws ServerException {
		Session session = validateSession();

		try {
			ContextProperties conf = Context.get().getProperties();
			conf.setProperty(session.getTenantName() + ".index.excludes",
					searchEngine.getExcludePatters() != null ? searchEngine.getExcludePatters() : "");
			conf.setProperty(session.getTenantName() + ".index.includes",
					searchEngine.getIncludePatters() != null ? searchEngine.getIncludePatters() : "");
			conf.setProperty(session.getTenantName() + ".index.excludes.metadata",
					searchEngine.getExcludePatternsMetadata() != null ? searchEngine.getExcludePatternsMetadata() : "");
			conf.setProperty(session.getTenantName() + ".index.includes.metadata",
					searchEngine.getIncludePattersMetadata() != null ? searchEngine.getIncludePattersMetadata() : "");
			conf.setProperty(session.getTenantName() + ".index.skiponerror",
					Boolean.toString(searchEngine.isSkipOnError()));
			conf.setProperty(session.getTenantName() + ".parser.timeout",
					searchEngine.getParsingTimeout() != null ? Integer.toString(searchEngine.getParsingTimeout()) : "");
			conf.setProperty(session.getTenantName() + ".parser.timeout.retain",
					Boolean.toString(searchEngine.isParsingTimeoutRetain()));
			conf.setProperty(session.getTenantName() + ".parser.txt.maxsize",
					searchEngine.getMaxTextFileSize() != null ? Integer.toString(searchEngine.getMaxTextFileSize())
							: "");

			if (session.getTenantId() == Tenant.DEFAULT_ID) {
				conf.setProperty("index.batch", Integer.toString(searchEngine.getBatch()));
				conf.setProperty("index.maxtext", Integer.toString(searchEngine.getMaxText()));
				conf.setProperty("index.dir", searchEngine.getDir());
				conf.setProperty("index.sorting", searchEngine.getSorting());
				conf.setProperty("index.sorting.custom", searchEngine.getCustomSorting());
				conf.setProperty("threadpool.IndexerTask.max", "" + searchEngine.getThreads());
			}

			conf.write();
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void setLanguageStatus(String language, boolean active) throws ServerException {
		Session session = validateSession();

		try {
			ContextProperties conf = Context.get().getProperties();
			conf.setProperty(session.getTenantName() + ".lang." + language, active ? "enabled" : "disabled");
			conf.write();
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void setAliases(String extension, String aliases) throws ServerException {
		Session session = validateSession();

		try {
			StringTokenizer st = new StringTokenizer(aliases, ",", false);
			List<String> buf = new ArrayList<>();
			while (st.hasMoreElements())
				buf.add(((String) st.nextElement()).trim());

			ParserFactory.setAliases(extension, buf.toArray(new String[0]));
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public long countEntries() throws ServerException {
		Session session = validateSession();
		try {
			SearchEngine indexer = Context.get(SearchEngine.class);
			return indexer.getCount();
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public void reorderTokenFilters(List<String> filters) throws ServerException {
		Session session = validateSession();
		try {
			ContextProperties conf = Context.get().getProperties();
			int i = 1;
			for (String filter : filters)
				conf.setProperty(INDEX_TOKENFILTER + filter + ".position", Integer.toString(i++));
			conf.write();
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void saveTokenFilterSettings(String filter, List<GUIParameter> settings) throws ServerException {
		Session session = validateSession();
		try {
			String prefix = INDEX_TOKENFILTER + filter + ".";
			ContextProperties conf = Context.get().getProperties();
			for (GUIParameter setting : settings)
				conf.setProperty(prefix + setting.getName(), setting.getValue().trim());
			conf.write();
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void setTokenFilterStatus(String filter, boolean active) throws ServerException {
		Session session = validateSession();
		try {
			ContextProperties conf = Context.get().getProperties();
			conf.setProperty(INDEX_TOKENFILTER + filter, active ? "enabled" : "disabled");
			conf.write();
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void purge() throws ServerException {
		Session session = validateSession();

		try {
			executeLongRunningOperation("Purge Index", () -> {
				SearchEngine indexer = Context.get(SearchEngine.class);
				indexer.purge();
				return null;
			}, session);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public void remove(List<Long> entryIds) throws ServerException {
		Session session = validateSession();

		try {
			Callable<Void> callable = () -> {
				SearchEngine indexer = Context.get(SearchEngine.class);

				indexer.deleteHits(entryIds);
				log.info("Removed {} entries from the index", entryIds.size());

				DocumentDAO dao = Context.get(DocumentDAO.class);
				StringBuilder updateQuery = new StringBuilder();
				updateQuery = new StringBuilder("update ld_document set ld_indexed=0 where ld_indexed = 1 ");

				StringBuilder hitsIdsCondition = new StringBuilder();
				if (!entryIds.isEmpty()) {
					hitsIdsCondition.append(" and (");

					if (dao.isOracle()) {
						/*
						 * In Oracle The limit of 1000 elements applies to sets
						 * of single items: (x) IN ((1), (2), (3), ...). There
						 * is no limit if the sets contain two or more items:
						 * (x, 0) IN ((1,0), (2,0), (3,0), ...):
						 */
						hitsIdsCondition.append(" (ld_id,0) in ( ");
						hitsIdsCondition.append(
								entryIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
						hitsIdsCondition.append(" )");
					} else {
						hitsIdsCondition.append(" ld_id in " + entryIds.toString().replace('[', '(').replace(']', ')'));
					}

					hitsIdsCondition.append(")");
				}
				updateQuery.append(hitsIdsCondition.toString());

				try {
					int updated = dao.jdbcUpdate(updateQuery.toString());
					log.info("{} documents marked to be indexed", updated);
				} catch (PersistenceException e) {
					log.error(e.getMessage(), e);
				}
				return null;
			};

			executeLongRunningOperation("Delete Index Entries", callable, session);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public GUIResult query(String query, int page, int size) throws ServerException {
		Session session = validateSession();
		try {
			SearchEngine indexer = Context.get(SearchEngine.class);
			Hits hits = indexer.query(query, page, size);

			GUIResult result = new GUIResult();
			result.setEstimatedHits(hits.getEstimatedCount());
			result.setTime(hits.getElapsedTime());

			List<GUIDocument> guiResults = new ArrayList<>();

			Map<Long, Hit> hitsMap = new HashMap<>();
			while (hits.hasNext()) {
				Hit hit = hits.next();
				hitsMap.put(hit.getId(), hit);
			}

			executeEnrichingQuery(hitsMap);

			// Now sort the hits by score desc
			List<Hit> sortedHitsList = new ArrayList<>(hitsMap.values());
			sortedHitsList.sort((h1, h2) -> Long.compare(h1.getId(), h2.getId()));
			for (Hit hit : sortedHitsList)
				guiResults.add(toDocument(hit));
			result.setHits(guiResults);

			return result;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	private GUIDocument toDocument(Hit hit) {
		GUIDocument document = new GUIDocument();
		try {
			document.setId(hit.getId());
			document.setTenantId(hit.getTenantId());
			document.setCustomId(hit.getCustomId());
			document.setType(hit.getType());
			document.setFileName(hit.getFileName());
			document.setColor(hit.getColor());
			document.setVersion(hit.getVersion());
			document.setCreation(hit.getCreation());
			document.setCreator(hit.getCreator());
			document.setCreatorId(hit.getCreatorId());
			document.setDate(hit.getDate());
			document.setPublisher(hit.getPublisher());
			document.setPublisherId(hit.getPublisherId());
			document.setFileVersion(hit.getFileVersion());
			document.setLanguage(hit.getLanguage());
			document.setTemplateId(hit.getTemplateId());
			document.setLastModified(hit.getLastModified());
			document.setLockUserId(hit.getLockUserId());
			document.setLockUser(hit.getLockUser());
			document.setComment(hit.getComment());
			document.setStatus(hit.getStatus().ordinal());
			document.setWorkflowStatus(hit.getWorkflowStatus());
			document.setWorkflowStatusDisplay(hit.getWorkflowStatusDisplay());
			document.setImmutable(hit.getImmutable());
			document.setFileSize(hit.getFileSize());
			document.setStartPublishing(hit.getStartPublishing());
			document.setStopPublishing(hit.getStopPublishing());
			document.setPublished(hit.getPublished());
			document.setSigned(hit.getSigned());
			document.setStamped(hit.getStamped());
			document.setIndexed(hit.getIndexed().ordinal());
			document.setExtResId(hit.getExtResId());
			document.setPages(hit.getPages());
			document.setPreviewPages(hit.getPreviewPages());
			document.setNature(hit.getNature());
			document.setFormId(hit.getFormId());
			document.setIcon(FileUtil.getBaseName(hit.getIcon()));
			document.setPasswordProtected(hit.isPasswordProtected());
			document.setLinks(hit.getLinks());
			document.setDocAttrs(hit.getDocAttrs());
			document.setOcrd(hit.getOcrd());
			document.setOcrTemplateId(hit.getOcrTemplateId());
			document.setBarcoded(hit.getBarcoded());
			document.setBarcodeTemplateId(hit.getBarcodeTemplateId());

			if (hit.getRating() != null)
				document.setRating(hit.getRating());

			if (hit.getCustomId() != null)
				document.setCustomId(hit.getCustomId());
			else
				document.setCustomId("");

			if (hit.getFolder() != null) {
				GUIFolder fold = new GUIFolder();
				fold.setId(hit.getFolder().getId());
				fold.setName(hit.getFolder().getName());
				document.setFolder(fold);
			}
		} catch (Exception t) {
			document.setId(hit.getId());
			document.setLanguage(hit.getLanguage());
			GUIFolder fold = new GUIFolder();
			fold.setId(hit.getFolder().getId());
			fold.setName(hit.getFolder().getName());
			document.setFolder(fold);
		}
		return document;
	}

	private void executeEnrichingQuery(Map<Long, Hit> hitsMap) throws SearchException {
		// Find real documents
		StringBuilder richQuery = new StringBuilder(
				"select A.ld_id, A.ld_customid, A.ld_docref, A.ld_type, A.ld_version, A.ld_lastmodified, ");
		richQuery.append(" A.ld_date, A.ld_publisher, A.ld_creation, A.ld_creator, A.ld_filesize, A.ld_immutable, ");
		richQuery.append(" A.ld_indexed, A.ld_lockuserid, A.ld_filename, A.ld_status, A.ld_signed, A.ld_type, ");
		richQuery.append(" A.ld_rating, A.ld_fileversion, A.ld_comment, A.ld_workflowstatus, A.ld_startpublishing, ");
		richQuery.append(" A.ld_stoppublishing, A.ld_published, ");
		richQuery.append(
				" FOLD.ld_name, A.ld_folderid, A.ld_tgs tags, A.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
		richQuery.append(
				" A.ld_stamped, A.ld_password, A.ld_workflowstatusdisp, A.ld_language, A.ld_pages, A.ld_color ");
		richQuery.append(" from ld_document A ");
		richQuery.append(" join ld_folder FOLD on A.ld_folderid=FOLD.ld_id ");
		richQuery.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");
		richQuery.append(" where A.ld_deleted=0 and A.ld_folderid=FOLD.ld_id  ");

		DocumentDAO dao = Context.get(DocumentDAO.class);

		Set<Long> hitsIds = hitsMap.keySet();
		StringBuilder hitsIdsCondition = new StringBuilder();
		if (!hitsIds.isEmpty()) {
			hitsIdsCondition.append(" and (");

			if (dao.isOracle()) {
				/*
				 * In Oracle The limit of 1000 elements applies to sets of
				 * single items: (x) IN ((1), (2), (3), ...). There is no limit
				 * if the sets contain two or more items: (x, 0) IN ((1,0),
				 * (2,0), (3,0), ...):
				 */
				hitsIdsCondition.append(" (A.ld_id,0) in ( ");
				hitsIdsCondition
						.append(hitsIds.stream().map(id -> ("(" + id + ",0)")).collect(Collectors.joining(",")));
				hitsIdsCondition.append(" )");
			} else {
				hitsIdsCondition.append(" A.ld_id in " + hitsIds.toString().replace('[', '(').replace(']', ')'));
			}

			hitsIdsCondition.append(")");
		}
		richQuery.append(hitsIdsCondition.toString());

		log.debug("Execute query {}", richQuery);

		try {
			dao.query(richQuery.toString(), new HitMapper(hitsMap), null);
		} catch (PersistenceException e) {
			throw new SearchException(e);
		}
	}

	class HitMapper implements RowMapper<Hit> {

		private Map<Long, Hit> hitsMap;

		public HitMapper(Map<Long, Hit> hitsMap) {
			super();
			this.hitsMap = hitsMap;
		}

		public Hit mapRow(ResultSet rs, int rowNum) throws SQLException {
			Hit hit = hitsMap.get(rs.getLong(1));
			if (hit == null) {
				// This is an alias
				hit = new Hit();
				hitsMap.put(rs.getLong(1), hit);
			}

			// Maintain the ID stored in the index
			hit.setCustomId(rs.getString(2));
			if (rs.getLong(3) != 0L) {
				hit.setDocRef(rs.getLong(3));
				Hit master = hitsMap.get(rs.getLong(3));
				if (master != null) {
					hit.setContent(master.getContent());
					hit.setSummary(master.getSummary());
				}
				hit.setDocRefType(rs.getString(33));
			}
			hit.setType(rs.getString(4));
			hit.setVersion(rs.getString(5));
			hit.setLastModified(rs.getTimestamp(6));
			hit.setDate(rs.getTimestamp(7));
			hit.setPublisher(rs.getString(8));
			hit.setCreation(rs.getTimestamp(9));
			hit.setCreator(rs.getString(10));
			hit.setFileSize(rs.getLong(11));
			hit.setImmutable(rs.getInt(12));
			hit.setIndexingStatus(rs.getInt(13));
			hit.setLockUserId(rs.getLong(14));
			hit.setFileName(rs.getString(15));
			hit.setStatus(rs.getInt(16));
			hit.setSigned(rs.getInt(17));
			hit.setType(rs.getString(18));
			hit.setRating(rs.getInt(19));
			hit.setFileVersion(rs.getString(20));
			hit.setComment(rs.getString(21));
			hit.setWorkflowStatus(rs.getString(22));
			hit.setStartPublishing(rs.getTimestamp(23));
			hit.setStopPublishing(rs.getTimestamp(24));
			hit.setPublished(rs.getInt(25));

			Folder folder = new Folder();
			folder.setName(rs.getString(26));
			folder.setId(rs.getLong(27));
			hit.setFolder(folder);

			if (rs.getLong(29) != 0L) {
				Template t = new Template();
				t.setId(rs.getLong(29));
				t.setName(rs.getString(30));
				hit.setTemplate(t);
				hit.setTemplateId(t.getId());
			}

			// Maintain the Tenant ID stored in the index
			hit.setStamped(rs.getInt(33));
			hit.setPassword(rs.getString(34));
			hit.setWorkflowStatusDisplay(rs.getString(35));
			// Maintain the language stored in the index
			hit.setPages(rs.getInt(37));
			hit.setColor(rs.getString(38));

			return hit;
		}
	}
}