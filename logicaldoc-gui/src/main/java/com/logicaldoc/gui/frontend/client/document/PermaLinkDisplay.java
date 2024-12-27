package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.http.client.URL;
import com.google.gwt.widgetideas.graphics.client.ImageLoader;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;

/**
 * Displays the permalinks of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class PermaLinkDisplay extends Window {

	private static final int QR_SIZE = 100;

	public PermaLinkDisplay(long docId) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("permalink"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		String urlBase = Session.get().getConfig("server.url");
		if (!urlBase.endsWith("/"))
			urlBase += "/";

		String downloadUrl = Util.downloadURL(docId);
		String downloadUrl2 = urlBase + "download?docId=" + docId;

		String displayUrl = Util.displayURL(docId, null);
		String displayUrl2 = urlBase + "display?docId=" + docId;

		ImageLoader.loadImages(new String[] { Util.qrURL(downloadUrl, QR_SIZE), Util.qrURL(downloadUrl2, QR_SIZE),
				Util.qrURL(displayUrl, QR_SIZE), Util.qrURL(displayUrl2, QR_SIZE) }, imageElements -> {
					StaticTextItem downloadUrlItem = prepareBarcodeAndLink("download1", "download", downloadUrl);
					downloadUrlItem.setShowTitle(true);
					StaticTextItem downloadUrlItem2 = prepareBarcodeAndLink("download2", "download", downloadUrl2);
					downloadUrlItem2.setShowTitle(false);
					downloadUrlItem2.setVisible(!downloadUrl.equals(downloadUrl2));

					StaticTextItem displayUrlItem = prepareBarcodeAndLink("displayUrl1", "details", displayUrl);
					displayUrlItem.setShowTitle(true);
					StaticTextItem displayUrlItem2 = prepareBarcodeAndLink("displayUrl2", "details", displayUrl2);
					displayUrlItem2.setShowTitle(false);
					displayUrlItem2.setVisible(!displayUrl.equals(displayUrl2));

					DynamicForm form = new DynamicForm();
					form.setNumCols(1);
					form.setMargin(2);
					form.setTitleOrientation(TitleOrientation.TOP);
					form.setItems(downloadUrlItem, downloadUrlItem2, displayUrlItem, displayUrlItem2);

					addItem(form);
				});
	}

	private StaticTextItem prepareBarcodeAndLink(String name, String label, String url) {
		StaticTextItem item = ItemFactory.newStaticTextItem(name, I18N.message(label),
				"<table border='0'><tr><td>" + Util.qrImg(URL.encode(url), QR_SIZE) + "</td><td><a href='" + url
						+ "' target='_blank'>" + url + "</a></td></tr></table>");
		item.setWrap(false);
		item.setWrapTitle(false);
		return item;
	}
}