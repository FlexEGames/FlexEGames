package me.hsgamer.flexegames.game.duel.world;

import me.hsgamer.minigamecore.base.Arena;
import net.kyori.adventure.text.Component;
import net.minestom.server.coordinate.Pos;
import net.minestom.server.instance.Instance;
import net.minestom.server.item.ItemStack;

import java.util.List;

public interface DuelWorld {
    List<Pos> getPos();

    Pos getJoinPos();

    Instance createInstance(Arena arena);

    ItemStack getDisplayItem();

    Component getDisplayName();
}
