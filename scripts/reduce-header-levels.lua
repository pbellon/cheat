local min_level = 99

function compute_min_level(header)
  if header.level < min_level then
    min_level = header.level
  end
end

function change_level(block, i)
  if (block.t == "Header") then
    block.level = block.level + (1 - min_level)
  end
  return block
end

function process(doc)
  doc.blocks = doc.blocks:map(change_level)
  return doc
end

return {
  {Header = compute_min_level},
  {Pandoc = process}
}
