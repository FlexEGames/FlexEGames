package me.hsgamer.flexegames.api.game;

import me.hsgamer.minigamecore.base.GameState;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

/**
 * A custom {@link GameState} that displays the name as a {@link Component}
 */
public interface ComponentGameState extends GameState {
    /**
     * Get the name of the state as a {@link Component}
     *
     * @return the name
     */
    default Component getDisplayNameAsComponent() {
        return LegacyComponentSerializer.legacyAmpersand().deserialize(getDisplayName());
    }
}
