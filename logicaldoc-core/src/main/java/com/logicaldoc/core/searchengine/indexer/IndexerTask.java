package com.logicaldoc.core.searchengine.indexer;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistentObjectDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.task.AbstractDocumentProcessor;
import com.logicaldoc.core.task.DocumentProcessorCallable;
import com.logicaldoc.core.task.DocumentProcessorStats;
import com.logicaldoc.core.task.TaskException;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.util.time.TimeDiff;

import jakarta.annotation.Resource;

/**
 * This task enlists all non-indexed documents and performs the indexing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
@Component("indexerTask")
public class IndexerTask extends AbstractDocumentProcessor {

	public static final String NAME = "IndexerTask";

	@Resource(name = "SearchEngine")
	protected SearchEngine searchEngine;

	private long totalIndexedDocuments = 0;

	private long totalErrors = 0;

	private long totalIndexingTime = 0;

	private long totalParsingTime = 0L;

	private List<Indexer> threads = new ArrayList<>();

	public IndexerTask() {
		super(NAME);
		log = LoggerFactory.getLogger(IndexerTask.class);
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return true;
	}

	@Override
	public synchronized void interrupt() {
		super.interrupt();
		for (Indexer thread : threads)
			thread.interrupt();
	}

	void onDocumentProcessed() {
		next();
	}

	@Override
	public void runTask() throws TaskException {
		if (searchEngine.isLocked()) {
			log.warn("Index locked, skipping indexing");
			return;
		}

		log.info("Start indexing of all documents");

		totalErrors = 0;
		totalIndexedDocuments = 0;
		totalIndexingTime = 0;
		totalParsingTime = 0;

		try {
			super.runTask();
		} finally {
			if (log.isInfoEnabled()) {
				log.info("Indexing finished");
				log.info("Indexing time: {}", TimeDiff.printDuration(totalIndexingTime));
				log.info("Parsing time: {}", TimeDiff.printDuration(totalParsingTime));
				log.info("Indexed documents: {}", totalIndexedDocuments);
				log.info("Errors: {}", totalErrors);
			}

			searchEngine.unlock();
		}
	}

	@Override
	protected void allCompleted(List<DocumentProcessorStats> stats) {
		for (DocumentProcessorStats stat : stats) {
			IndexerStats iStat = (IndexerStats) stat;
			totalIndexedDocuments += iStat.getProcessed();
			totalErrors += iStat.getErrors();
			totalIndexingTime += iStat.getIndexingTime();
			totalParsingTime += iStat.getParsingTime();
		}
	}

	/**
	 * Prepares the query to search the documents to be indexed
	 * 
	 * @return array composed by the query to execute and the sorting expression
	 */
	public static String[] prepareQuery() {
		ContextProperties config = Context.get().getProperties();

		// Determine the sorting
		String sorting = config.getProperty("index.sorting");
		if (StringUtils.isNotEmpty(sorting)) {
			if ("oldestfirst".equals(sorting))
				sorting = PersistentObjectDAO.ENTITY + ".date asc";
			else if ("mostrecentfirst".equals(sorting))
				sorting = PersistentObjectDAO.ENTITY + ".date desc";
			else if ("smallestfirst".equals(sorting))
				sorting = PersistentObjectDAO.ENTITY + ".fileSize asc";
			else
				sorting = PersistentObjectDAO.ENTITY + ".fileSize desc";
		}

		// This hidden setting overrides the default sorting policy(some really
		// demanding users may need this optimization).
		String sortingCustom = config.getProperty("index.sorting.custom");
		if (StringUtils.isNotEmpty(sortingCustom))
			sorting = sortingCustom;

		String where = PersistentObjectDAO.ENTITY + ".deleted = 0 and (" + PersistentObjectDAO.ENTITY
				+ ".indexingStatus = " + IndexingStatus.TO_INDEX.ordinal() + " or " + PersistentObjectDAO.ENTITY
				+ ".indexingStatus = " + IndexingStatus.TO_INDEX_METADATA.ordinal() + ") and not "
				+ PersistentObjectDAO.ENTITY + ".status = " + DocumentStatus.ARCHIVED.ordinal();

		return new String[] { where, sorting };
	}

	@Override
	public String prepareReport(Locale locale) {
		StringBuilder sb = new StringBuilder();
		sb.append(I18N.message("indexationtime", locale) + ": ");
		sb.append(TimeDiff.printDuration(totalIndexingTime));
		sb.append("\n");
		sb.append(I18N.message("indexeddocs", locale) + ": ");
		sb.append(totalIndexedDocuments);
		sb.append("\n");
		sb.append(I18N.message("errors", locale) + ": ");
		sb.append(totalErrors);
		return sb.toString();
	}

	@Override
	protected String getDefaultUser() {
		return "_system";
	}

	@Override
	protected int getBatchSize() {
		return config.getInt("index.batch", 500);
	}

	@Override
	protected void prepareQueueQuery(StringBuilder where, StringBuilder sort) {
		String[] queryParts = prepareQuery();
		where.append(queryParts[0]);
		sort.append(queryParts[1]);
	}

	@Override
	protected DocumentProcessorCallable<IndexerStats> prepareCallable(List<Long> segment) {
		return new Indexer(segment, this, log);
	}
}