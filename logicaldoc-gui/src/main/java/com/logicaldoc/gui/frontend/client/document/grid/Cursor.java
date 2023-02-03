package com.logicaldoc.gui.frontend.client.document.grid;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;

/**
 * A cursor to browse among pages
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class Cursor extends ToolStrip {

	private Label label = null;

	private SpinnerItem pageSizeItem;

	private SpinnerItem pageItem;

	private int totalRecords = 0;

	public Cursor() {
		this(false, false);
	}

	/**
	 * ID of the message to be used to compose the email
	 *
	 * @param enabledPagination if the pagination must be enabled
	 * @param compactView if the compact visualization must be used
	 */
	public Cursor(boolean enabledPagination, boolean compactView) {
		setHeight(20);

		label = new Label(" ");
		label.setWrap(false);
		label.setMargin(2);
		setAlign(Alignment.RIGHT);

		String mx = Session.get().getConfig("gui.document.pagesize");
		pageSizeItem = ItemFactory.newSpinnerItem("max", "display", Integer.parseInt(mx), 2, (Integer) null);
		pageSizeItem.setValue(Integer.parseInt(mx));
		pageSizeItem.setWidth(70);
		pageSizeItem.setStep(20);
		pageSizeItem.setSaveOnEnter(true);
		pageSizeItem.setImplicitSave(true);
		pageSizeItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onPageSizeChange();
			}
		});
		if (compactView) {
			pageSizeItem.setShowTitle(false);
		} else {
			pageSizeItem.setHint(I18N.message("elements"));
		}

		addFormItem(pageSizeItem);
		if (enabledPagination) {
			pageItem = ItemFactory.newSpinnerItem("page", "page", 1, 1, 1);
			pageItem.setHint("");
			pageItem.setSaveOnEnter(true);
			pageItem.setImplicitSave(true);

			addSeparator();
			addFormItem(pageItem);
		}
		addFill();
		addMember(label);
	}

	public void setMessage(String message) {
		label.setContents(message);
	}

	public void setTotalRecords(int totalRecords) {
		this.totalRecords = totalRecords;
		if (pageItem == null)
			return;
		int max = getPageSize();
		int pages = (int) Math.ceil((double) totalRecords / (double) max);
		if (pages == 0)
			pages = 1;
		if (getCurrentPage() > pages)
			setCurrentPage(pages);

		pageItem.setMax(pages);
		pageItem.setHint("/" + pages);
	}

	public int getPageSize() {
		return Integer.parseInt(pageSizeItem.getValue().toString());
	}

	public void setPageSize(int maxRecords) {
		pageSizeItem.setValue(maxRecords);
	}

	public int getCurrentPage() {
		if (pageItem == null)
			return 1;
		return Integer.parseInt(pageItem.getValue().toString());
	}

	public int getTotalPages() {
		if (pageItem == null)
			return 1;
		return Integer.parseInt(pageItem.getMax().toString());
	}

	public void setCurrentPage(int page) {
		if (pageItem == null)
			return;
		pageItem.setValue(page);
	}

	private void onPageSizeChange() {
		if (Boolean.TRUE.equals(pageSizeItem.validate())) {
			setCurrentPage(1);
			setTotalRecords(totalRecords);
		}
	}

	public void registerPageSizeChangedHandler(ChangedHandler handler) {
		pageSizeItem.addChangedHandler(handler);
	}

	public void registerPageChangedHandler(ChangedHandler handler) {
		if (pageItem != null)
			pageItem.addChangedHandler(handler);
	}

	public void showTitle(boolean showTitle) {
		pageSizeItem.setShowTitle(showTitle);
		pageSizeItem.setShowHint(showTitle);
	}

	public void showStackedIcons(boolean stackedIcons) {
		pageSizeItem.setWriteStackedIcons(stackedIcons);
	}
}