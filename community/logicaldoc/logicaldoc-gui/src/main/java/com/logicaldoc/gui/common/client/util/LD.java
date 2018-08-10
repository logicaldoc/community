package com.logicaldoc.gui.common.client.util;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;

/**
 * This class contains useful methods for objects visualization.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class LD {

	/**
	 * Show a dialog to confirm a operation.
	 */
	public static void ask(String title, String message, Integer width, final BooleanCallback callback) {
		final Window dialog = new Window();

		dialog.setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		dialog.setAutoCenter(true);
		dialog.setIsModal(true);
		dialog.setShowModalMask(true);
		dialog.setShowHeader(true);
		dialog.setAutoSize(true);
		dialog.setCanDragResize(false);
		dialog.setCanDrag(true);
		dialog.centerInPage();
		dialog.setTitle(title);
		if (width != null)
			dialog.setWidth(width);
		else
			dialog.setWidth(300);

		VLayout container = new VLayout();
		container.setWidth100();
		container.setMembersMargin(5);
		container.setMargin(3);
		container.setAlign(Alignment.CENTER);
		container.setDefaultLayoutAlign(Alignment.CENTER);

		DynamicForm textForm = new DynamicForm();
		textForm.setTitleOrientation(TitleOrientation.TOP);
		textForm.setAlign(Alignment.CENTER);
		textForm.setNumCols(1);
		StaticTextItem text = ItemFactory.newStaticTextItem("text", "", message);
		text.setShouldSaveValue(false);
		text.setWrapTitle(false);
		text.setAlign(Alignment.CENTER);
		text.setShowTitle(false);
		textForm.setFields(text);

		IButton yes = new IButton(I18N.message("yes"));
		yes.setWidth(70);
		yes.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (callback != null) {
					dialog.close();
					callback.execute(true);
					dialog.destroy();
				}
			}
		});

		IButton no = new IButton(I18N.message("no"));
		no.setWidth(70);
		no.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (callback != null) {
					dialog.close();
					callback.execute(false);
					dialog.destroy();
				}
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(10);
		buttons.setWidth100();
		buttons.setAutoHeight();
		buttons.addMember(yes);
		buttons.addMember(no);
		buttons.setAlign(Alignment.CENTER);

		container.addMember(textForm);
		container.addMember(buttons);

		dialog.addItem(container);
		dialog.show();
	}

	public static void ask(String title, String message, final BooleanCallback callback) {
		ask(title, message, null, callback);
	}

	/**
	 * Show a dialog asking for a value to complete a operation.
	 */
	public static void askForValue(String title, String message, String defaultValue, Integer width,
			ValueCallback callback) {
		TextItem textItem = ItemFactory.newTextItem("value", message, defaultValue);
		askForValue(title, message, defaultValue, textItem, width, callback);
	}

	public static void askForDocumentPassword(String title, String message, Integer width, final ValueCallback callback) {
		final Window dialog = new Window();
		dialog.setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		dialog.setAutoCenter(true);
		dialog.setAutoSize(true);
		dialog.setIsModal(true);
		dialog.setShowModalMask(true);
		dialog.setShowHeader(true);
		dialog.setCanDragResize(false);
		dialog.setCanDrag(true);
		dialog.centerInPage();
		dialog.setTitle(title);
		if (width != null)
			dialog.setWidth(width);
		else
			dialog.setWidth(300);

		VStack container = new VStack();
		container.setWidth100();
		container.setMembersMargin(5);
		container.setMargin(3);
		container.setAlign(Alignment.CENTER);
		container.setDefaultLayoutAlign(Alignment.CENTER);

		final DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setWrapItemTitles(false);
		form.setNumCols(1);

		PasswordItem item = ItemFactory.newPasswordItem("value", message, null);
		item.setWidth("100%");
		item.setName("value");
		item.setTitle(I18N.message(message));
		item.setWrapTitle(false);
		item.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (form.validate() && event.getKeyName() != null && "enter".equals(event.getKeyName().toLowerCase())) {
					if (callback != null) {
						dialog.close();
						callback.execute(form.getValue("value").toString());
						dialog.destroy();
					}
				}
			}
		});

		form.setFields(item);

		IButton ok = new IButton(I18N.message("ok"));
		ok.setAutoFit(true);
		ok.setMinWidth(70);
		ok.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (form.validate() && callback != null) {
					dialog.close();
					callback.execute(form.getValue("value").toString());
					dialog.destroy();
				}
			}
		});

		IButton cancel = new IButton(I18N.message("cancel"));
		cancel.setAutoFit(true);
		cancel.setMinWidth(70);
		cancel.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (callback != null) {
					dialog.close();
					callback.execute(null);
					dialog.destroy();
				}
			}
		});

		IButton unset = new IButton(I18N.message("unsetpassword"));
		unset.setAutoFit(true);
		unset.setMinWidth(70);
		unset.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (callback != null) {
					dialog.close();
					callback.execute("--unset--");
					dialog.destroy();
				}
			}
		});

		HLayout buttons = new HLayout();
		buttons.setAlign(Alignment.CENTER);
		buttons.setMembersMargin(10);
		buttons.setWidth100();
		buttons.setAutoHeight();
		buttons.addMember(ok);
		buttons.addMember(cancel);
		if (Session.get().getUser().isMemberOf("admin"))
			buttons.addMember(unset);

		container.addMember(form);
		container.addMember(buttons);

		dialog.addItem(container);
		dialog.show();

		form.focusInItem(item);
		form.setAutoFocus(true);
		form.focus();
	}

	public static void askForValue(String title, String message, String defaultValue, ValueCallback callback) {
		askForValue(title, message, defaultValue, (Integer) null, callback);
	}

	/**
	 * Show a dialog asking for a value to complete an operation. The provided
	 * form item will be used.
	 */
	public static void askForValue(String title, String message, String defaultValue, FormItem item, Integer width,
			final ValueCallback callback) {
		final Window dialog = new Window();

		dialog.setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		dialog.setAutoCenter(true);
		dialog.setAutoSize(true);
		dialog.setIsModal(true);
		dialog.setShowModalMask(true);
		dialog.setShowHeader(true);
		dialog.setCanDragResize(false);
		dialog.setCanDrag(true);
		dialog.centerInPage();
		dialog.setTitle(title);
		if (width != null)
			dialog.setWidth(width);
		else
			dialog.setWidth(300);

		VStack container = new VStack();
		container.setWidth100();
		container.setMembersMargin(5);
		container.setMargin(3);
		container.setAlign(Alignment.CENTER);
		container.setDefaultLayoutAlign(Alignment.CENTER);

		final DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setWrapItemTitles(false);
		form.setNumCols(1);

		item.setWidth("100%");
		item.setName("value");
		item.setTitle(I18N.message(message));
		item.setWrapTitle(false);
		if (!(item instanceof TextAreaItem))
			item.addKeyPressHandler(new KeyPressHandler() {
				@Override
				public void onKeyPress(KeyPressEvent event) {
					if (form.validate() && event.getKeyName() != null
							&& "enter".equals(event.getKeyName().toLowerCase())) {
						if (callback != null) {
							dialog.close();
							callback.execute(form.getValue("value").toString());
							dialog.destroy();
						}
					}
				}
			});

		form.setFields(item);

		IButton ok = new IButton(I18N.message("ok"));
		ok.setAutoFit(true);
		ok.setMinWidth(70);
		ok.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (form.validate() && callback != null) {
					dialog.close();
					callback.execute(form.getValue("value").toString());
					dialog.destroy();
				}
			}
		});

		IButton cancel = new IButton(I18N.message("cancel"));
		cancel.setAutoFit(true);
		cancel.setMinWidth(70);
		cancel.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (callback != null) {
					dialog.close();
					callback.execute(null);
					dialog.destroy();
				}
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(10);
		buttons.setWidth100();
		buttons.setAutoHeight();
		buttons.addMember(ok);
		buttons.addMember(cancel);
		buttons.setAlign(Alignment.CENTER);

		container.addMember(form);
		container.addMember(buttons);

		dialog.addItem(container);
		dialog.show();

		if (defaultValue != null) {
			item.setValue(defaultValue);
			if (item instanceof TextItem) {
				((TextItem) item).selectValue();
				if (defaultValue.length() > 0)
					((TextItem) item).setSelectionRange(0, defaultValue.length());
			} else {
				form.focusInItem(item);
				form.setAutoFocus(true);
				form.focus();
			}
		}
	}

	public static void askforValue(String title, String message, String defaultValue, FormItem item,
			final ValueCallback callback) {
		askForValue(title, message, defaultValue, item, null, callback);
	}
}