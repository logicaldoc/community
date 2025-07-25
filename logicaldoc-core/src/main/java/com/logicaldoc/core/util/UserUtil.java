package com.logicaldoc.core.util;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.imaging.ImageUtil;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserType;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;
import com.talanlabs.avatargenerator.Avatar;
import com.talanlabs.avatargenerator.IdenticonAvatar;
import com.timgroup.jgravatar.Gravatar;
import com.timgroup.jgravatar.GravatarDefaultImage;
import com.timgroup.jgravatar.GravatarRating;

/**
 * Various utilities about users.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class UserUtil {

	private static final String AVATAR = "avatar";

	private static final String ERROR_GENERATING_DEFAULT_THE_AVATAR_FOR_USER = "Error generating default the avatar for user {}";

	/**
	 * A transparent 1x1 PNG
	 */
	private static final String TRANSPARENT_IMAGE = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=";

	private static final Logger log = LoggerFactory.getLogger(UserUtil.class);

	private UserUtil() {
	}

	/**
	 * This method retrieves the user folder. If not exists, it creates the
	 * folder. The folder is: <b>conf.userdir</b>/<b>id</b>
	 * 
	 * @param id The user identifier
	 * 
	 * @return the user's data folder
	 */
	public static File getUserHome(long id) {
		File root = getUsersDir();
		File userDir = new File(root, Long.toString(id));
		if (!userDir.exists()) {
			try {
				FileUtils.forceMkdir(userDir);
			} catch (IOException e) {
				return null;
			}
		}
		return userDir;
	}

	/**
	 * This method retrieves a user resource (file or folder). If the resource
	 * is a folder and not exists, it creates the folder. The folder will be:
	 * <b>conf.userdir</b>/<b>id</b>/<b>path</b>
	 * 
	 * @param id The user identifier
	 * @param path The resource path
	 * 
	 * @return the user's resource
	 */
	public static File getUserResource(long id, String path) {
		File root = getUserHome(id);
		File resource = new File((root != null ? root.getPath() : "") + "/" + path);

		if (!resource.exists() && !path.contains("."))
			try {
				FileUtils.forceMkdir(resource);
			} catch (IOException e) {
				return null;
			}

		return resource;
	}

	/**
	 * This method retrieves the users root folder. If not exists, it creates
	 * the folder. The folder is: <b>conf.userdir</b>
	 * 
	 * @return the root folder that contains the users data folders
	 */
	public static File getUsersDir() {
		File userpath = new File("");
		try {
			ContextProperties conf = Context.get().getProperties();
			userpath = new File(conf.getProperty("conf.userdir"));
			FileUtils.forceMkdir(userpath);
		} catch (Exception t) {
			// Nothing to do
		}
		return userpath;
	}

	/**
	 * Updates the avatar of a user with a given .png image
	 * 
	 * @param user The user to elaborate
	 * @param avatarImageFile The file containing the avatar image
	 * @param avatarImageFile The image type(eg png, svg)
	 */
	public static void saveAvatar(User user, File avatarImageFile, String imageType) {
		UserDAO userDao = Context.get(UserDAO.class);
		TenantDAO tenantDao = Context.get(TenantDAO.class);

		File tmpAvatarImage = null;
		try {
			userDao.initialize(user);
			if ("svg".equalsIgnoreCase(imageType)) {
				// In case of SVG we save the image as is
				user.setAvatar("data:image/svg+xml;base64," + ImageUtil.encode(avatarImageFile));
			} else {
				// In case of raster image we crop and resize
				String tenantName = tenantDao.getTenantName(user.getTenantId());
				int size = Context.get().getProperties().getInt(tenantName + ".gui.avatar.size", 128);
				tmpAvatarImage = FileUtil.createTempFile(AVATAR, ".png");
				BufferedImage avatar = ImageIO.read(avatarImageFile);
				avatar = ImageUtil.cropCenterSquare(avatar, size);
				ImageIO.write(avatar, "png", tmpAvatarImage);
				user.setAvatar("data:image/png;base64," + ImageUtil.encode(tmpAvatarImage));
			}

			userDao.store(user);
		} catch (Exception t) {
			log.warn(ERROR_GENERATING_DEFAULT_THE_AVATAR_FOR_USER, user, t);
		} finally {
			FileUtil.delete(tmpAvatarImage);
		}
	}

	/**
	 * Updates the avatar of a user with the default avatar that is the Gravatar
	 * or the auto-generated image(in case Gravatar is not available)
	 * 
	 * @param user The user to elaborate
	 */
	public static void generateDefaultAvatar(User user) {
		UserDAO userDao = Context.get(UserDAO.class);

		File tmpAvatarImage = null;
		try {
			userDao.initialize(user);
			tmpAvatarImage = FileUtil.createTempFile(AVATAR, ".png");

			TenantDAO tenantDao = Context.get(TenantDAO.class);
			String tenantName = tenantDao.getTenantName(user.getTenantId());
			int size = Context.get().getProperties().getInt(tenantName + ".gui.avatar.size", 128);

			BufferedImage avatar = UserUtil.generateDefaultAvatarImage(user, size);
			ImageIO.write(avatar, "png", tmpAvatarImage);
			user.setAvatar("data:image/png;base64," + ImageUtil.encode(tmpAvatarImage));

			if (user.getType() != UserType.SYSTEM) {
				userDao.store(user);
			} else {
				Map<String, Object> params = new HashMap<>();
				params.put(AVATAR, user.getAvatar());
				params.put("username", user.getUsername());
				userDao.jdbcUpdate("update ld_user set ld_avatar = :avatar where ld_username = :username", params);
			}
		} catch (Exception t) {
			if (log.isDebugEnabled())
				log.debug(ERROR_GENERATING_DEFAULT_THE_AVATAR_FOR_USER, user, t);
		} finally {
			FileUtil.delete(tmpAvatarImage);
		}
	}

	private static BufferedImage generateDefaultAvatarImage(User user, int size) {
		/*
		 * Check Gravatar with main email
		 */
		BufferedImage avatarImage = getImageFromGravatar(user, size);

		/*
		 * Now generate one from scratch
		 */
		if (avatarImage == null) {
			try {
				Avatar avatar = IdenticonAvatar.newAvatarBuilder().size(size, size).build();
				avatarImage = avatar.create(user.getId() > 0 ? user.getId() : -user.getId());
			} catch (Exception t) {
				log.warn("Cannot generate avatar for user {}", user, t);
			}
		}

		avatarImage = getImageCentralSquare(size, avatarImage);

		return avatarImage;
	}

	protected static BufferedImage getImageFromGravatar(User user, int size) {
		if (!Context.get().getProperties().getBoolean("gravatar.enabled", false))
			return null;

		BufferedImage avatarImage = null;
		Gravatar gravatar = new Gravatar();
		gravatar.setRating(GravatarRating.GENERAL_AUDIENCES);
		gravatar.setDefaultImage(GravatarDefaultImage.GRAVATAR_ICON);
		gravatar.setSize(size);

		byte[] bytes;
		try {
			bytes = gravatar.download(user.getEmail());
			if (bytes != null && bytes.length > 0)
				avatarImage = ImageIO.read(new ByteArrayInputStream(bytes));
		} catch (Exception t) {
			log.warn("Cannot download gravatar for email {}", user.getEmail(), t);
		}

		/*
		 * Check Gravatar with secondary email
		 */
		if (avatarImage == null && StringUtils.isNotEmpty(user.getEmail2())) {
			try {
				bytes = gravatar.download(user.getEmail2());
				if (bytes != null && bytes.length > 0)
					avatarImage = ImageIO.read(new ByteArrayInputStream(bytes));
			} catch (Exception t) {
				log.warn("Cannot download gravatar for email {}", user.getEmail2(), t);
			}
		}
		return avatarImage;
	}

	/**
	 * If the image is bigger, then gets the central square
	 * 
	 * @param size Wanted size of the output image
	 * @param avatarImage the avatar image
	 * 
	 * @return the central square
	 */
	private static BufferedImage getImageCentralSquare(int size, BufferedImage avatarImage) {
		if (avatarImage != null && (avatarImage.getWidth() > size || avatarImage.getHeight() > size)) {
			avatarImage = ImageUtil.cropCenterSquare(avatarImage, size);
		}
		return avatarImage;
	}

	public static String getAvatarImage(String userIdOrName) throws PersistenceException {
		String content = TRANSPARENT_IMAGE;

		UserDAO userDao = Context.get(UserDAO.class);
		User user = null;
		if (userIdOrName != null)
			try {
				user = userDao.findById(Long.parseLong(userIdOrName));
			} catch (Exception t) {
				// perhaps the id is the username
				user = userDao.findByUsername(userIdOrName);
			}

		if (user != null) {
			if (StringUtils.isEmpty(user.getAvatar()))
				UserUtil.generateDefaultAvatar(user);
			content = user.getAvatar();
		}

		return content;
	}
}