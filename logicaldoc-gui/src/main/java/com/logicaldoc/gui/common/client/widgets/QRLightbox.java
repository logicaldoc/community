package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.widgetideas.graphics.client.ImageLoader;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ImageStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;

/**
 * A light box to show a barcode. If the barcode is a link and the provided
 * content
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class QRLightbox extends DelayedRedrawWindow {
	private static final String QRCODE = "qrcode";

	private static final int QR_SIZE = 150;

	public QRLightbox(String content) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message(QRCODE));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		String qrUrl1 = Util.qrURL(content, QR_SIZE);
		if ((content.startsWith("http") || content.startsWith("https")) && content.startsWith(Util.contextPath())
				&& !content.startsWith(Session.get().getConfig("server.url"))) {

			// We are not accessing from the declared server.url so display the
			// additional QR with server.url
			final StringBuilder content2 = new StringBuilder(Session.get().getConfig("server.url"));
			if (!content2.toString().endsWith("/"))
				content2.append("/");
			content2.append(content.substring(Util.contextPath().length()));

			String qrUrl2 = Util.qrURL(content2.toString(), QR_SIZE);

			ImageLoader.loadImages(new String[] { qrUrl1, qrUrl2 }, imageElements -> {
				StaticTextItem qr1Item = ItemFactory.newStaticTextItem("qr1", QRCODE,
						"<table border='0'><tr><td><img src='" + imageElements[0].getSrc() + "' />"
								+ "</td><td><a href='" + content + "' target='_blank'>" + content
								+ "</a></td></tr></table>");
				qr1Item.setWrap(false);
				qr1Item.setWrapTitle(false);
				qr1Item.setShowTitle(false);

				StaticTextItem qr2Item = ItemFactory.newStaticTextItem("qr2", QRCODE,
						"<table border='0'><tr><td><img src='" + imageElements[1].getSrc() + "' />"
								+ "</td><td><a href='" + content2 + "' target='_blank'>" + content2
								+ "</a></td></tr></table>");
				qr2Item.setWrap(false);
				qr2Item.setWrapTitle(false);
				qr2Item.setShowTitle(false);

				DynamicForm form = new DynamicForm();
				form.setMargin(2);
				form.setNumCols(1);
				form.setTitleOrientation(TitleOrientation.LEFT);
				form.setItems(qr1Item, qr2Item);
				addItem(form);

				delayedRedraw();
			});
		} else {
			// We are accessing from the declared server.url so just display the
			// QR
			ImageLoader.loadImages(new String[] { qrUrl1 }, imageElements -> {
				Img img = new Img(qrUrl1);
				img.setImageWidth(QR_SIZE);
				img.setImageHeight(QR_SIZE);
				img.setImageType(ImageStyle.NORMAL);
				addItem(img);
			});
		}
	}
}