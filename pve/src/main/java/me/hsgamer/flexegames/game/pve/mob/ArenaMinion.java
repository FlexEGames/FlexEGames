package me.hsgamer.flexegames.game.pve.mob;

import net.minestom.server.entity.EntityType;
import org.jetbrains.annotations.NotNull;

public class ArenaMinion extends ArenaMob {
    private final ArenaMob owner;

    public ArenaMinion(@NotNull EntityType entityType, @NotNull ArenaMob owner) {
        super(entityType, owner.arena);
        this.owner = owner;
    }

    public ArenaMob owner() {
        return owner;
    }
}