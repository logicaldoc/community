package com.logicaldoc.webdav.resource;

import java.io.IOException;

import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.io.OutputContext;
import org.eclipse.jetty.http.HttpHeader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webdav.context.ExportContext;
import com.logicaldoc.webdav.exception.UncheckedDavException;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.session.WebdavSession;
import com.logicaldoc.webdav.web.AbstractWebdavServlet;

public class RangeResourceImpl extends DavResourceImpl {

	private static final long serialVersionUID = 3923284739563663530L;

	private static final Logger log = LoggerFactory.getLogger(RangeResourceImpl.class);

	protected static final String CONTENT_TYPE_VALUE = "application/octet-stream";

	protected static final String CONTENT_DISPOSITION_HEADER = "Content-Disposition";

	protected static final String CONTENT_DISPOSITION_VALUE = "attachment";

	protected static final String X_CONTENT_TYPE_OPTIONS_HEADER = "X-Content-Type-Options";

	protected static final String X_CONTENT_TYPE_OPTIONS_VALUE = "nosniff";

	private final Pair<String, String> requestRange;

	public RangeResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfiguration config, Resource resource, Pair<String, String> requestRange) {
		super(locator, factory, session, config, resource);
		this.requestRange = requestRange;
	}

	@Override
	public void spool(OutputContext outputContext) throws IOException {

		log.debug("RangeResourceImpl.spool()");

		outputContext.setModificationTime(this.resource.getLastModified().getTime());
		outputContext.setETag(this.resource.getETag());
		outputContext.setContentType(AbstractWebdavServlet.getContext().getMimeType(this.resource.getName()));

		if (!outputContext.hasStream()) {
			return;
		}

		final long contentLength = this.resource.getContentLength();
		final Pair<Long, Long> range = getEffectiveRange(contentLength);
		if (range.getLeft() < 0 || range.getLeft() > range.getRight() || range.getRight() > contentLength) {
			log.debug("REQUESTED_RANGE_NOT_SATISFIABLE");
			if (log.isDebugEnabled())
				log.debug("Content-Range: {}", "bytes */" + contentLength);

			outputContext.setProperty(HttpHeader.CONTENT_RANGE.asString(), "bytes */" + contentLength);
			throw new UncheckedDavException(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE,
					"Valid Range would be in [0, " + contentLength + "]");
		}

		final Long rangeLength = range.getRight() - range.getLeft() + 1;
		outputContext.setContentLength(rangeLength);
		outputContext.setProperty(HttpHeader.CONTENT_RANGE.asString(),
				contentRangeResponseHeader(range.getLeft(), range.getRight(), contentLength));
		log.debug("Content-Range: bytes {}-{}/{}", range.getLeft(), range.getRight(), contentLength);

		outputContext.setProperty(CONTENT_DISPOSITION_HEADER, CONTENT_DISPOSITION_VALUE);
		outputContext.setProperty(X_CONTENT_TYPE_OPTIONS_HEADER, X_CONTENT_TYPE_OPTIONS_VALUE);

		if (exists()) {
			ExportContext exportCtx = getExportContext(outputContext);

			if (!config.getIOManager().exportContent(exportCtx, this, range.getLeft(), rangeLength)) {
				throw new IOException(
						"Unexpected Error while spooling resource - " + range.getLeft() + "-" + rangeLength);
			}
		}
	}

	private String contentRangeResponseHeader(long firstByte, long lastByte, long completeLength) {
		return String.format("bytes %d-%d/%d", firstByte, lastByte, completeLength);
	}

	private Pair<Long, Long> getEffectiveRange(long contentLength) {
		try {
			final Long lower = requestRange.getLeft().isEmpty() ? null : Long.valueOf(requestRange.getLeft());
			final Long upper = requestRange.getRight().isEmpty() ? null : Long.valueOf(requestRange.getRight());
			if (lower == null && upper == null) {
				return new ImmutablePair<>(0l, contentLength - 1);
			} else if (lower == null) {
				return new ImmutablePair<>(contentLength - upper, contentLength - 1);
			} else if (upper == null) {
				return new ImmutablePair<>(lower, contentLength - 1);
			} else {
				return new ImmutablePair<>(lower, Math.min(upper, contentLength - 1));
			}
		} catch (NumberFormatException e) {
			throw new IllegalArgumentException("Invalid byte range: " + requestRange, e);
		}
	}

}
