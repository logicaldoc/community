package com.logicaldoc.core.document.dao;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;

/**
 * Hibernate implementation of <code>DocumentDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 3.0
 */
public class HibernateVersionDAO extends HibernatePersistentObjectDAO<Version> implements VersionDAO {

	private Storer storer;

	private FolderDAO folderDAO;

	private HibernateVersionDAO() {
		super(Version.class);
		super.log = LoggerFactory.getLogger(HibernateVersionDAO.class);
	}

	@Override
	public List<Version> findByDocId(long docId) {
		try {
			return findByWhere(" _entity.docId=" + docId, "order by _entity.versionDate desc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
			return new ArrayList<Version>();
		}

	}

	@Override
	public Version findByVersion(long docId, String version) {
		List<Version> versions = new ArrayList<Version>();
		try {
			versions = findByWhere(" _entity.docId=" + docId + " and _entity.version='" + version + "'", null, null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (!versions.isEmpty())
			return versions.get(0);
		else
			return null;
	}

	@Override
	public Version findByFileVersion(long docId, String fileVersion) {
		List<Version> versions = new ArrayList<Version>();
		try {
			versions = findByWhere(" _entity.docId=" + docId + " and _entity.fileVersion='" + fileVersion + "'",
					"order by _entity.date asc", null);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (!versions.isEmpty())
			return versions.get(0);
		else
			return null;
	}

	@Override
	public void initialize(Version version) {
		refresh(version);

		for (String attribute : version.getAttributes().keySet()) {
			attribute.getBytes();
		}
	}

	@Override
	public boolean store(Version version) {
		boolean result = true;
		try {
			result = super.store(version);
			if (!result)
				return false;

			// Checks the context property 'document.maxversions'
			ContextProperties bean = new ContextProperties();
			int maxVersions = bean.getInt("document.maxversions");
			Folder workspace = folderDAO.findWorkspace(version.getFolderId());
			if (workspace != null && workspace.getMaxVersions() != null && workspace.getMaxVersions() > 0)
				maxVersions = workspace.getMaxVersions();

			if (maxVersions > 0) {
				List<Version> versions = findByDocId(version.getDocId());
				// Inverse order the document versions
				if (versions.size() > maxVersions) {
					Collections.sort(versions, new Comparator<Version>() {
						public int compare(Version v1, Version v2) {
							return v2.compareTo(v1);
						}
					});

					// Prepare a list of files(fileVersion) that must be
					// retained
					Set<String> filesToBeRetained = new HashSet<String>();
					for (int i = 0; i < versions.size(); i++) {
						if (i < maxVersions)
							if (!filesToBeRetained.contains(versions.get(i).getFileVersion()))
								filesToBeRetained.add(versions.get(i).getFileVersion());
					}

					// Delete the oldest versions
					for (int i = 0; i < versions.size(); i++) {
						if (i >= maxVersions) {
							// Delete the version
							Version deleteVersion = versions.get(i);
							initialize(deleteVersion);
							deleteVersion.setDeleted(1);
							store(deleteVersion);
						}
					}

					// Clean the files no more needed
					List<String> resources = storer.listResources(version.getDocId(), null);
					for (String resource : resources) {
						boolean toDelete = true;
						for (String fileVersionToRetain : filesToBeRetained) {
							if (resource.trim().equals(fileVersionToRetain.trim())
									|| resource.trim().startsWith(fileVersionToRetain.trim() + "-")) {
								toDelete = false;
								break;
							}
						}
						if (toDelete) {
							storer.delete(version.getDocId(), resource);
						}
					}
				}
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	@Override
	public void updateDigest(Version version) {
		initialize(version);
		String resource = storer.getResourceName(version.getDocId(), version.getFileVersion(), null);
		if (storer.exists(version.getDocId(), resource)) {
			InputStream in = null;
			try {
				in = storer.getStream(version.getDocId(), resource);
				version.setDigest(FileUtil.computeDigest(in));
			} catch (IOException e) {
				log.error("Cannot retrieve the content of version {}", version, e);
			} finally {
				if (in != null)
					try {
						in.close();
					} catch (Throwable t) {
					}
			}
			saveOrUpdate(version);
		}
	}

	public void setStorer(Storer storer) {
		this.storer = storer;
	}

	@Override
	public boolean delete(long versionId, int delCode) {
		assert (delCode != 0);

		if (!checkStoringAspect())
			return false;

		boolean result = true;
		try {
			Version ver = (Version) findById(versionId);
			assert (ver != null);
			if (ver != null) {
				ver.setDeleted(delCode);
				// TODO At the moment the version ld_version column is just 10
				// chars, we have to change it to varchar(255)
				ver.setVersion(StringUtils.right(versionId + "." + ver.getVersion(), 10));
				store(ver);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			result = false;
		}
		return result;
	}

	public void setFolderDAO(FolderDAO folderDAO) {
		this.folderDAO = folderDAO;
	}
}