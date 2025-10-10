package com.logicaldoc.core.folder;

import java.util.List;

import jakarta.annotation.Resource;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskException;

/**
 * This task calculate the path attributes of the folders(only those folders
 * without path will be processed.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 */
@Component("pathCalculator")
public class PathCalculator extends Task {

	public static final String NAME = "PathCalculator";
	
	@Resource(name = "folderDAO")
	protected FolderDAO folderDao;

	private long processed = 0;

	private long errors = 0;

	public PathCalculator() {
		super(NAME);
		log = LoggerFactory.getLogger(PathCalculator.class);
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return false;
	}

	@Override
	protected void runTask() throws TaskException {
		log.info("Start indexing of all documents");

		errors = 0;
		processed = 0;

		try {
			// First of all find folders to be processed and not already
			// involved into a transaction
			List<Long> ids = folderDao
					.queryForList("select ld_id from ld_folder where ld_deleted=0 and ld_path is null", Long.class);
			log.info("Found a total of {} folders to be processed", ids.size());
			setSize(ids.size());

			if (!ids.isEmpty()) {
				for (Long id : ids) {
					processFolder(id);
					if (interruptRequested)
						break;
				}
			}
		} catch (PersistenceException e) {
			throw new TaskException(e.getMessage(), e);
		} finally {
			log.info("Path calculation finished");
			log.info("Processed folders: {}", processed);
			log.info("Errors: {}", errors);
		}
	}

	private void processFolder(Long id) {
		try {
			String path = folderDao.computePath(id);
			folderDao.jdbcUpdate("update ld_folder set ld_path='" + path + "' where ld_id=" + id);
			processed++;
		} catch (Exception e) {
			log.error("Error processing folder {}: {}", id, e.getMessage());
			log.error(e.getMessage(), e);
			errors++;
		} finally {
			next();
		}
	}

	public FolderDAO getFolderDao() {
		return folderDao;
	}

}