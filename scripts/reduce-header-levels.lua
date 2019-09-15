local min_level = 99

function is_header(block)
  return block.t == "Header"
end

function compute_min_level(blocks)
  for block in pairs(blocks) do
    if (is_header(block) and block.level < min_level) then
      min_level = block.level
    end
  end
end

function is_prism_line(line)
  print("Is line a prism one ?" .. line)
  return 
end

function is_prism_block(block)
  local str = pandoc.utils.stringify(block)
  local index = string.find(str, "{:")
  return not (index == nil)
end

function change_level(block, i)
  if (is_prism_block(block)) then
    return nil
  elseif (is_header(block)) then
    if (block.level < min_level) then
      return nil
    else
      block.level = block.level + (1 - min_level)
    end
  end
  return block
end

function is_not_nil(el)
  return not (el == nil)
end

return {
  {
    Pandoc = function (doc)
      
      -- compute_min_level(doc.blocks)
      min_level = 3
      doc.blocks = doc.blocks:map(change_level):filter(is_not_nil)
      return doc
    end
  }
}
