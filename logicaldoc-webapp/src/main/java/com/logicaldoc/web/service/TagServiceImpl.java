package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.TagCloud;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITag;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Implementation of the TagService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TagServiceImpl extends AbstractRemoteService implements TagService {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(TagServiceImpl.class);

	@Override
	public List<GUITag> getTagCloud() throws ServerException {
		Session session = validateSession();
		try {
			ArrayList<GUITag> ret = new ArrayList<>();
			DocumentDAO dao = Context.get(DocumentDAO.class);
			List<TagCloud> list = dao.getTagCloud(session.getSid());

			for (TagCloud tagCloud : list) {
				GUITag c = new GUITag();
				c.setScale(tagCloud.getScale());
				c.setTag(tagCloud.getTag());
				c.setCount(tagCloud.getCount());
				ret.add(c);
			}

			return ret;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public void delete(String tag) {
		// Nothing to do
	}

	@Override
	public void rename(String tag, String newTag) {
		// Nothing to do
	}

	class TagCloudComparatorName implements Comparator<TagCloud> {
		public int compare(TagCloud tc0, TagCloud tc1) {
			return tc0.getTag().compareTo(tc1.getTag());
		}
	}

	@Override
	public void addTag(String tag) throws ServerException {
		// Nothing to do
	}

	@Override
	public void removeTag(String tag) throws ServerException {
		// Nothing to do
	}

	@Override
	public List<GUIParameter> getSettings() throws ServerException {
		Session session = validateSession();

		ContextProperties conf = Context.get().getProperties();
		List<GUIParameter> params = new ArrayList<>();
		for (Object name : conf.keySet()) {
			if (name.toString().startsWith(session.getTenantName() + ".tag.")
					|| name.toString().startsWith(session.getTenantName() + ".tagcloud.")) {
				if (name.equals(session.getTenantName() + ".tag.mode"))
					params.add(new GUIParameter(name.toString(), "free"));
				else
					params.add(new GUIParameter(name.toString(), conf.getProperty(name.toString())));
			}
		}

		return params;
	}
}