package com.logicaldoc.gui.frontend.client.document.grid;

import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.Offline;
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

	private String maxCookieName = CookiesManager.COOKIE_DOCSLIST_MAX;

	private SpinnerItem maxItem;

	private SpinnerItem pageItem;

	public Cursor() {
		this(null, false, false);
	}

	/**
	 * ID of the message to be used to compose the email
	 * 
	 * @param maxDisplayedRecordsCookieName name of the cookies to save the max number of displayed records
	 * @param enabledPagination if the pagination must be enabled
	 * @param compactView if the compact vidualization must be used
	 */
	public Cursor(String maxDisplayedRecordsCookieName, boolean enabledPagination, boolean compactView) {
		setHeight(20);
		this.maxCookieName = maxDisplayedRecordsCookieName;

		label = new Label(" ");
		label.setWrap(false);
		label.setMargin(2);
		setAlign(Alignment.RIGHT);

		String mx = "100";
		if (maxDisplayedRecordsCookieName != null) {
			if (Offline.get(maxDisplayedRecordsCookieName) != null
					&& !Offline.get(maxDisplayedRecordsCookieName).equals(""))
				mx = (String) Offline.get(maxDisplayedRecordsCookieName);
		}

		maxItem = ItemFactory.newSpinnerItem("max", "display", Integer.parseInt(mx), 2, (Integer) null);
		maxItem.setValue(Integer.parseInt(mx));
		maxItem.setWidth(70);
		maxItem.setStep(20);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onMaxChange();
			}
		});
		if (compactView) {
			maxItem.setShowTitle(false);
		} else {
			maxItem.setHint(I18N.message("elements"));
		}

		addFormItem(maxItem);
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
		if (pageItem == null)
			return;
		int max = getMaxDisplayedRecords();
		int pages = (int) Math.ceil((double) totalRecords / (double) max);
		if (pages == 0)
			pages = 1;
		if (getCurrentPage() > pages)
			setCurrentPage(pages);

		pageItem.setMax(pages);
		pageItem.setHint("/" + pages);
	}

	public int getMaxDisplayedRecords() {
		return Integer.parseInt(maxItem.getValue().toString());
	}

	public void setMaxDisplayedRecords(int maxRecords) {
		maxItem.setValue(maxRecords);
	}

	public int getCurrentPage() {
		if (pageItem == null)
			return 1;
		return Integer.parseInt(pageItem.getValue().toString());
	}

	public void setCurrentPage(int page) {
		if (pageItem == null)
			return;
		pageItem.setValue(page);
	}

	private void onMaxChange() {
		if (maxItem.validate() && maxCookieName != null) {
			CookiesManager.save(maxCookieName, maxItem.getValueAsString());
			setCurrentPage(1);
		}
	}

	public void registerMaxChangedHandler(ChangedHandler handler) {
		maxItem.addChangedHandler(handler);
	}

	public void registerPageChangedHandler(ChangedHandler handler) {
		if (pageItem != null)
			pageItem.addChangedHandler(handler);
	}

	public void showTitle(boolean showTitle) {
		maxItem.setShowTitle(showTitle);
		maxItem.setShowHint(showTitle);
	}

	public void showStackedIcons(boolean stackedIcons) {
		maxItem.setWriteStackedIcons(stackedIcons);
	}
}