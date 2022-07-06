package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.document.TagCloud;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITag;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the TagService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TagServiceImpl extends RemoteServiceServlet implements TagService {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(TagServiceImpl.class);

	@Override
	public GUITag[] getTagCloud() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			ArrayList<GUITag> ret = new ArrayList<GUITag>();
			DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			List<TagCloud> list = dao.getTagCloud(session.getSid());

			for (TagCloud tagCloud : list) {
				GUITag c = new GUITag();
				c.setScale(tagCloud.getScale());
				c.setTag(tagCloud.getTag());
				c.setCount(tagCloud.getCount());
				ret.add(c);
			}

			return ret.toArray(new GUITag[0]);
		} catch (Throwable t) {
			return (GUITag[]) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void delete(String tag) {

	}

	@Override
	public void rename(String tag, String newTag) {

	}

	class TagCloudComparatorName implements Comparator<TagCloud> {
		public int compare(TagCloud tc0, TagCloud tc1) {
			return tc0.getTag().compareTo(tc1.getTag());
		}
	}

	@Override
	public void addTag(String tag) throws ServerException {

	}

	@Override
	public void removeTag(String tag) throws ServerException {

	}

	@Override
	public GUIParameter[] getSettings() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		ContextProperties conf = Context.get().getProperties();
		List<GUIParameter> params = new ArrayList<GUIParameter>();
		for (Object name : conf.keySet()) {
			if (name.toString().startsWith(session.getTenantName() + ".tag.")
					|| name.toString().startsWith(session.getTenantName() + ".tagcloud."))
				if (name.equals(session.getTenantName() + ".tag.mode"))
					params.add(new GUIParameter(name.toString(), "free"));
				else
					params.add(new GUIParameter(name.toString(), conf.getProperty(name.toString())));
		}

		return params.toArray(new GUIParameter[0]);
	}
}