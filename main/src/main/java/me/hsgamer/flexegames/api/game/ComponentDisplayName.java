package me.hsgamer.flexegames.api.game;

import me.hsgamer.minigamecore.base.extra.DisplayName;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

public interface ComponentDisplayName extends DisplayName {
    /**
     * Get the name as a {@link Component}
     *
     * @return the name
     */
    default Component getDisplayNameAsComponent() {
        return LegacyComponentSerializer.legacyAmpersand().deserialize(getDisplayName());
    }
}
