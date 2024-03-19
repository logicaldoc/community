package com.logicaldoc.gui.common.client.util;

import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
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

	private static final String CONTACTINGSERVER = "contactingserver";

	private static final String VALUE = "value";

	private LD() {
	}

	/**
	 * Show a dialog to confirm a operation
	 * 
	 * @param title title of the dialog box
	 * @param message text printed in the body of the dialog box
	 * @param width width dimension expressed in pixels
	 * @param callback a call back invoked when the user confirm his choice
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
		yes.addClickHandler(event -> {
			if (callback != null) {
				dialog.close();
				callback.execute(true);
				dialog.destroy();
			}
		});

		IButton no = new IButton(I18N.message("no"));
		no.setWidth(70);
		no.addClickHandler(event -> {
			if (callback != null) {
				dialog.close();
				callback.execute(false);
				dialog.destroy();
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
	 * Show a dialog asking for a value to complete a operation
	 * 
	 * @param title title of the dialog box
	 * @param message text printed in the body of the dialog box
	 * @param defaultValue default value
	 * @param width width of the dialog box
	 * @param callback call back used when the user confirms the input
	 */
	public static void askForValue(String title, String message, String defaultValue, Integer width,
			ValueCallback callback) {
		TextItem textItem = ItemFactory.newTextItem(VALUE, message, defaultValue);
		askForValue(title, message, defaultValue, textItem, width, callback);
	}

	public static void askForDocumentPassword(String title, String message, Integer width,
			final ValueCallback callback) {
		final Window dialog = prepareDialogForDocumentPassword(title, width);

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

		PasswordItem item = ItemFactory.newPasswordItem(VALUE, message, null);
		item.setWidth("100%");
		item.setName(VALUE);
		item.setTitle(I18N.message(message));
		item.setWrapTitle(false);
		item.addKeyPressHandler((KeyPressEvent event) -> {
			if (form.validate() && event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName())
					&& callback != null) {
				dialog.close();
				callback.execute(form.getValue(VALUE).toString());
				dialog.destroy();
			}
		});

		form.setFields(item);

		IButton ok = new IButton(I18N.message("ok"));
		ok.setAutoFit(true);
		ok.setMinWidth(70);
		ok.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> {
			if (form.validate() && callback != null) {
				dialog.close();
				callback.execute(form.getValue(VALUE).toString());
				dialog.destroy();
			}
		});

		IButton cancel = new IButton(I18N.message("cancel"));
		cancel.setAutoFit(true);
		cancel.setMinWidth(70);
		cancel.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> {
			if (callback != null) {
				dialog.close();
				callback.execute(null);
				dialog.destroy();
			}
		});

		IButton unset = new IButton(I18N.message("unsetpassword"));
		unset.setAutoFit(true);
		unset.setMinWidth(70);
		unset.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> {
			if (callback != null) {
				dialog.close();
				callback.execute("--unset--");
				dialog.destroy();
			}
		});

		HLayout buttons = new HLayout();
		buttons.setAlign(Alignment.CENTER);
		buttons.setMembersMargin(10);
		buttons.setWidth100();
		buttons.setAutoHeight();
		buttons.addMember(ok);
		buttons.addMember(cancel);
		if (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN))
			buttons.addMember(unset);

		container.addMember(form);
		container.addMember(buttons);

		dialog.addItem(container);
		dialog.show();

		form.focusInItem(item);
		form.setAutoFocus(true);
		form.focus();
	}

	private static Window prepareDialogForDocumentPassword(String title, Integer width) {
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
		return dialog;
	}

	public static void askForValue(String title, String message, String defaultValue, ValueCallback callback) {
		askForValue(title, message, defaultValue, (Integer) null, callback);
	}

	/**
	 * Show a dialog asking for a set of values to complete an operation. The
	 * provided form items will be used
	 * 
	 * @param title title of the dialog box
	 * @param message text printed in the body of the dialog box
	 * @param items the items used to input the values
	 * @param width width of the dialog box
	 * @param callback call back used when the user confirms the input
	 * @param cancelCallback call back used when the user cancels the input
	 */
	@SuppressWarnings("unchecked")
	public static void askForValues(String title, String message, List<FormItem> items, Integer width,
			final ValueCallback callback, final ClickHandler cancelCallback) {
		final Window dialog = prepareDialogForAskValues(title, width);
		if (cancelCallback != null)
			dialog.addCloseClickHandler(click -> cancelCallback.onClick(null));

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

		prepareItemsForAskValues(message, items, callback, dialog, form);
		form.setFields(items.toArray(new FormItem[0]));

		IButton ok = new IButton(I18N.message("ok"));
		ok.setAutoFit(true);
		ok.setMinWidth(70);
		ok.addClickHandler(event -> {
			if (form.validate() && callback != null) {
				dialog.close();
				if (callback instanceof ValuesCallback) {
					((ValuesCallback) callback).execute(form.getValues());
				} else
					callback.execute(form.getValue(VALUE) != null ? form.getValue(VALUE).toString() : null);
				dialog.destroy();
			}
		});

		IButton cancel = new IButton(I18N.message("cancel"));
		cancel.setAutoFit(true);
		cancel.setMinWidth(70);
		cancel.addClickHandler(event -> {
			dialog.close();
			dialog.destroy();
			if (cancelCallback != null)
				cancelCallback.onClick(event);
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

		form.focusInItem(items.get(0));
		form.setAutoFocus(true);
		form.focus();
	}

	/**
	 * Show a dialog asking for a set of values to complete an operation. The
	 * provided form items will be used
	 * 
	 * @param title title of the dialog box
	 * @param message text printed in the body of the dialog box
	 * @param items the items used to input the values
	 * @param width width of the dialog box
	 * @param callback call back used when the user confirms the input
	 */
	public static void askForValues(String title, String message, List<FormItem> items, Integer width,
			final ValueCallback callback) {
		askForValues(title, message, items, width, callback, null);
	}

	private static void prepareItemsForAskValues(String message, List<FormItem> items, final ValueCallback callback,
			final Window dialog, final DynamicForm form) {
		for (FormItem item : items) {
			if (items.size() == 1) {
				item.setName(VALUE);
				if (message == null)
					item.setShowTitle(false);
				else
					item.setTitle(I18N.message(message));
			}

			item.setWidth("100%");
			item.setWrapTitle(false);
			if (!(item instanceof TextAreaItem) && !(item instanceof RichTextItem) && items.size() == 1
					&& callback != null) {
				/*
				 * In case of simple input item, when the user presses the Enter
				 * key, we consider he wants to confirm the input
				 */
				item.addKeyPressHandler((KeyPressEvent event) -> {
					if (form.validate() && event.getKeyName() != null && "enter".equalsIgnoreCase(event.getKeyName())) {
						dialog.close();
						callback.execute(form.getValue(VALUE).toString());
						dialog.destroy();
					}
				});
			}
		}
	}

	private static Window prepareDialogForAskValues(String title, Integer width) {
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
		dialog.setTitle(I18N.message(title));
		if (width != null)
			dialog.setWidth(width);
		else
			dialog.setWidth(300);
		return dialog;
	}

	/**
	 * Shows a dialog asking for a value to complete an operation. The provided
	 * form item will be used
	 * 
	 * @param title title of the dialog box
	 * @param message text printed in the body of the dialog box
	 * @param defaultValue default value
	 * @param item the item used to input the value
	 * @param width width of the dialog box
	 * @param callback call back used when the user confirms the input
	 */
	public static void askForValue(String title, String message, String defaultValue, FormItem item, Integer width,
			final ValueCallback callback) {
		askForValue(title, message, defaultValue, item, width, callback, null);
	}

	/**
	 * Shows a dialog asking for a value to complete an operation. The provided
	 * form item will be used
	 * 
	 * @param title title of the dialog box
	 * @param message text printed in the body of the dialog box
	 * @param defaultValue default value
	 * @param item the item used to input the value
	 * @param width width of the dialog box
	 * @param callback call back used when the user confirms the input
	 * @param cancelCallback call back used when the user cancels the input
	 */
	public static void askForValue(String title, String message, String defaultValue, FormItem item, Integer width,
			final ValueCallback callback, final ClickHandler cancelCallback) {
		askForValues(title, message, Arrays.asList(item), width, callback, cancelCallback);

		if (defaultValue != null) {
			item.setValue(defaultValue);
			if (item instanceof TextItem) {
				((TextItem) item).selectValue();
				if (defaultValue.length() > 0)
					((TextItem) item).setSelectionRange(0, defaultValue.length());
			}
		}
	}

	public static void askForValue(String title, String message, String defaultValue, FormItem item,
			final ValueCallback callback) {
		askForValue(title, message, defaultValue, item, null, callback);
	}

	public static void askForString(String title, String message, String defaultValue, final ValueCallback callback) {
		askForValue(title, message, defaultValue, new TextItem(), null, callback);
	}

	public static void askForStringMandatory(String title, String message, String defaultValue,
			final ValueCallback callback) {
		TextItem item = new TextItem();
		item.setRequired(true);
		askForValue(title, message, defaultValue, item, null, callback);
	}

	public static void clearPrompt() {
		SC.clearPrompt();
	}

	public static void prompt(String message) {
		Dialog properties = new Dialog();
		properties.setShowHeader(false);
		properties.setShowHeaderBackground(false);
		properties.setMembersMargin(0);
		SC.showPrompt("", I18N.message(message), properties);
	}

	public static void contactingServer() {
		Dialog properties = new Dialog();
		properties.setShowHeader(false);
		properties.setShowHeaderBackground(false);
		properties.setMembersMargin(0);
		properties.setBodyStyle(CONTACTINGSERVER);
		SC.showPrompt("", AwesomeFactory.getSpinnerIconHtml("pulse", I18N.message(CONTACTINGSERVER)), properties);
	}

	public static void updatingServer() {
		Dialog properties = new Dialog();
		properties.setShowHeader(false);
		properties.setShowHeaderBackground(false);
		properties.setMembersMargin(0);
		properties.setBodyStyle(CONTACTINGSERVER);
		SC.showPrompt("", AwesomeFactory.getSpinnerIconHtml("pulse", I18N.message("systemupdating")), properties);
	}
}